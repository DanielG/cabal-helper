-- Copyright (C) 2015,2017  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

{-|
Module      : CabalHelper.Compiletime.Compile
Description : Runtime compilation machinery
License     : AGPL-3
-}

module CabalHelper.Compiletime.Compile where

import Control.Applicative
import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Traversable
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.Version
import GHC.IO.Exception (IOErrorType(OtherError))
import Text.Printf
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import System.IO.Error
import System.IO.Temp
import Prelude

import Distribution.System (buildPlatform)
import Distribution.Text (display)

import Paths_cabal_helper (version)
import CabalHelper.Compiletime.Data
import CabalHelper.Compiletime.Log
import CabalHelper.Compiletime.Types
import CabalHelper.Shared.Common
import CabalHelper.Shared.Sandbox (getSandboxPkgDb)

data Compile = Compile {
      compCabalSourceDir :: Maybe CabalSourceDir,
      compPackageDb      :: Maybe PackageDbDir,
      compCabalVersion   :: Either String Version,
      compPackageDeps    :: [String]
    }

compileHelper :: Options -> Version -> FilePath -> FilePath -> IO (Either ExitCode FilePath)
compileHelper opts cabalVer projdir distdir = do
  case cabalPkgDb opts of
    Nothing ->
      run [ compileCabalSource
          , Right <$> MaybeT (cachedExe cabalVer)
          , compileSandbox
          , compileGlobal
          , cachedCabalPkg
          , MaybeT (Just <$> compilePrivatePkgDb)
          ]
    mdb ->
      run [ Right <$> MaybeT (cachedExe cabalVer)
          , liftIO $ compileWithPkg mdb cabalVer
          ]

 where
   run actions = fromJust <$> runMaybeT (msum actions)

   logMsg = "compiling helper with Cabal from "

-- for relaxed deps: find (sameMajorVersionAs cabalVer) . reverse . sort

   -- | Check if this version is globally available
   compileGlobal :: MaybeT IO (Either ExitCode FilePath)
   compileGlobal = do
       ver <- MaybeT $ find (== cabalVer) <$> listCabalVersions opts
       vLog opts $ logMsg ++ "user/global package-db"
       liftIO $ compileWithPkg Nothing ver

   -- | Check if this version is available in the project sandbox
   compileSandbox :: MaybeT IO (Either ExitCode FilePath)
   compileSandbox = do
       let ghcVer = ghcVersion opts
           mdb_path = getSandboxPkgDb projdir (display buildPlatform) =<< ghcVer
       sandbox <- PackageDbDir <$> MaybeT mdb_path
       ver <- MaybeT $ logIOError opts "compileSandbox" $
         find (== cabalVer) <$> listCabalVersions' opts (Just sandbox)
       vLog opts $ logMsg ++ "sandbox package-db"
       liftIO $ compileWithPkg (Just sandbox) ver


   -- | Check if we already compiled this version of cabal into a private
   -- package-db
   cachedCabalPkg :: MaybeT IO (Either ExitCode FilePath)
   cachedCabalPkg = do
       db_exists <- liftIO $ cabalVersionExistsInPkgDb opts cabalVer
       case db_exists of
         False -> mzero
         True -> do
             db@(PackageDbDir db_path)
                 <- liftIO $ getPrivateCabalPkgDb opts (Right cabalVer)
             vLog opts $ logMsg ++ "private package-db in " ++ db_path
             liftIO $ compileWithPkg (Just db) cabalVer

   -- | See if we're in a cabal source tree
   compileCabalSource :: MaybeT IO (Either ExitCode FilePath)
   compileCabalSource = do
       let cabalFile = projdir </> "Cabal.cabal"
       cabalSrc <- liftIO $ doesFileExist cabalFile
       let projdir' = CabalSourceDir projdir
       case cabalSrc of
         False -> mzero
         True -> liftIO $ do
           vLog opts $ "directory above distdir looks like cabal source tree (Cabal.cabal exists)"
           ver <- cabalFileVersion <$> readFile cabalFile
           vLog opts $ "compiling helper with local Cabal source tree"
           compileWithCabalTree ver projdir'

   -- | Compile the requested cabal version into an isolated package-db
   compilePrivatePkgDb :: IO (Either ExitCode FilePath)
   compilePrivatePkgDb = do
       db <- fst <$> installCabal opts (Right cabalVer) `E.catch`
             \(SomeException _) -> errorInstallCabal cabalVer distdir
       compileWithPkg (Just db) cabalVer

   compileWithCabalTree ver srcDir =
       compile distdir opts $ Compile {
                     compCabalSourceDir       = Just srcDir,
                     compPackageDb            = Nothing,
                     compCabalVersion         = Right ver,
                     compPackageDeps          = []
                   }

   compileWithPkg mdb ver =
       compile distdir opts $ Compile {
                     compCabalSourceDir       = Nothing,
                     compPackageDb            = mdb,
                     compCabalVersion         = Right ver,
                     compPackageDeps          = [cabalPkgId ver]
                   }

   cabalPkgId v = "Cabal-" ++ showVersion v

compile :: FilePath -> Options -> Compile -> IO (Either ExitCode FilePath)
compile distdir opts@Options {..} Compile {..} = do
    cnCabalSourceDir
        <- (canonicalizePath . cabalSourceDir) `traverse` compCabalSourceDir
    appdir <- appCacheDir

    let (outdir, exedir, exe, mchsrcdir) =
          case cnCabalSourceDir of
             Nothing  -> ( exeName compCabalVersion <.> "build"
                         , appdir
                         , appdir </> exeName compCabalVersion
                         , Nothing
                         )
             Just _   -> ( distdir </> "cabal-helper"
                         , distdir
                         , distdir </> "cabal-helper" </> "cabal-helper"
                         , Just $ distdir </> "cabal-helper"
                         )

    createDirectoryIfMissing True outdir
    createDirectoryIfMissing True exedir

    withHelperSources mchsrcdir $ \compCabalHelperSourceDir -> do

    vLog opts $ "sourcedir: " ++ compCabalHelperSourceDir
    vLog opts $ "outdir: " ++ outdir
    vLog opts $ "exe: " ++ exe

    let (mj1:mj2:mi:_) = case compCabalVersion of
                         Left _commitid -> [10000000, 0, 0]
                         Right (Version vs _) -> vs
    let ghc_opts = concat [
          [ "-outputdir", outdir
          , "-o", exe
          , "-optP-DCABAL_HELPER=1"
          , "-optP-DCH_MIN_VERSION_Cabal(major1,major2,minor)=(\
                   \   (major1)  < "++show mj1++" \
                   \|| (major1) == "++show mj1++" && (major2)  < "++show mj2++"\
                   \|| (major1) == "++show mj1++" && (major2) == "++show mj2++" && (minor) <= "++show mi++")"
          ],
          maybeToList $ ("-package-conf="++) <$> packageDbDir <$> compPackageDb,
          map ("-i"++) $ nub $ "":compCabalHelperSourceDir:maybeToList cnCabalSourceDir,

          if isNothing cnCabalSourceDir
             then [ "-hide-all-packages"
                  , "-package", "base"
                  , "-package", "containers"
                  , "-package", "directory"
                  , "-package", "filepath"
                  , "-package", "process"
                  , "-package", "bytestring"
                  , "-package", "ghc-prim"
                  ]
             else [],

          concatMap (\p -> ["-package", p]) compPackageDeps,
          [ "--make"
          ,  compCabalHelperSourceDir</>"CabalHelper"</>"Runtime"</>"Main.hs"
          ]
         ]

    rv <- callProcessStderr' opts Nothing ghcProgram ghc_opts
    return $ case rv of
               ExitSuccess -> Right exe
               e@(ExitFailure _) -> Left e

exeName :: Either String Version -> String
exeName (Left commitid) = intercalate "-"
    [ "cabal-helper" ++ showVersion version -- our ver
    , "CabalHEAD" ++ commitid
    ]
exeName (Right compCabalVersion) = intercalate "-"
    [ "cabal-helper" ++ showVersion version -- our ver
    , "Cabal" ++ showVersion compCabalVersion
    ]

callProcessStderr'
    :: Options -> Maybe FilePath -> FilePath -> [String] -> IO ExitCode
callProcessStderr' opts mwd exe args = do
  let cd = case mwd of
             Nothing -> []; Just wd -> [ "cd", formatProcessArg wd++";" ]
  vLog opts $ intercalate " " $ cd ++ map formatProcessArg (exe:args)
  (_, _, _, h) <- createProcess (proc exe args) { std_out = UseHandle stderr
                                                , cwd = mwd }
  waitForProcess h

callProcessStderr :: Options -> Maybe FilePath -> FilePath -> [String] -> IO ()
callProcessStderr opts mwd exe args = do
  rv <- callProcessStderr' opts mwd exe args
  case rv of
    ExitSuccess -> return ()
    ExitFailure v -> processFailedException "callProcessStderr" exe args v

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fn exe args rv =
    ioError $ mkIOError OtherError msg Nothing Nothing
  where
    msg = concat [ fn, ": ", exe, " "
                 , intercalate " " (map formatProcessArg args)
                 , " (exit " ++ show rv ++ ")"
                 ]

formatProcessArg :: String -> String
formatProcessArg xs
    | any isSpace xs = "'"++ xs ++"'"
    | otherwise      = xs

data HEAD = HEAD deriving (Eq, Show)

installCabal :: Options -> Either HEAD Version -> IO (PackageDbDir, Either String Version)
installCabal opts ever = do
  appdir <- appCacheDir
  let message ver = do
      let sver = showVersion ver
      hPutStr stderr $ printf "\
\cabal-helper-wrapper: Installing a private copy of Cabal because we couldn't\n\
\find the right version in your global/user package-db, this might take a\n\
\while but will only happen once per Cabal version you're using.\n\
\\n\
\If anything goes horribly wrong just delete this directory and try again:\n\
\    %s\n\
\\n\
\If you want to avoid this automatic installation altogether install\n\
\version %s of Cabal manually (into your user or global package-db):\n\
\    $ cabal install Cabal --constraint \"Cabal == %s\"\n\
\\n\
\Installing Cabal %s ...\n" appdir sver sver sver

  withSystemTempDirectory "cabal-helper-Cabal-source" $ \tmpdir -> do
    (srcdir, e_commit_ver) <- case ever of
      Left HEAD -> do
        second Left <$> unpackCabalHEAD tmpdir
      Right ver -> do
        message ver
        let patch = fromMaybe nopCabalPatchDescription $
              find ((ver`elem`) . cpdVersions) patchyCabalVersions
        (,) <$> unpackPatchedCabal opts ver tmpdir patch <*> pure (Right ver)

    db <- createPkgDb opts e_commit_ver

    runCabalInstall opts db srcdir ever

    return (db, e_commit_ver)

{-
TODO: If the Cabal version we want to install is less than or equal to one we
have available, either through act-as-setup or in a package-db we should be able
to use act-as-setup or build a default Setup.hs exe and patch the Cabal source
to say build-type:simple. This will sidestep bugs in c-i>=1.24

See conversation in
https://github.com/haskell/cabal/commit/e2bf243300957321497353a2f85517e464f764ab

Otherwise we might be able to use the shipped Setup.hs

-}

runCabalInstall
    :: Options -> PackageDbDir -> CabalSourceDir -> Either HEAD Version-> IO ()
runCabalInstall opts (PackageDbDir db) (CabalSourceDir srcdir) ever = do
  civ@CabalInstallVersion {..} <- cabalInstallVersion opts
  cabal_opts <- return $ concat
      [
        [ "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--prefix=" ++ db </> "prefix"
        ]
        , cabalOptions opts
        , if cabalInstallVer >= Version [1,20,0,0] []
             then ["--no-require-sandbox"]
             else []
        , [ "install", srcdir ]
        , if verbose opts
            then ["-v"]
            else []
        , [ "--only-dependencies" ]
      ]

  callProcessStderr opts (Just "/") (cabalProgram opts) cabal_opts

  runSetupHs opts db srcdir ever civ

  hPutStrLn stderr "done"

cabalOptions :: Options -> [String]
cabalOptions opts =
    concat [ [ "--with-ghc=" ++ ghcProgram opts ]
           , if ghcPkgProgram opts /= ghcPkgProgram defaultOptions
               then [ "--with-ghc-pkg=" ++ ghcPkgProgram opts ]
               else []
           ]

runSetupHs
    :: Options
    -> FilePath
    -> FilePath
    -> Either HEAD Version
    -> CabalInstallVersion
    -> IO ()
runSetupHs opts@Options {..} db srcdir ever CabalInstallVersion {..}
    | cabalInstallVer >= parseVer "1.24" = do
      go $ \args -> callProcessStderr opts (Just srcdir) cabalProgram $
        [ "act-as-setup", "--" ] ++ args
    | otherwise = do
      SetupProgram {..} <- compileSetupHs opts db srcdir
      go $ callProcessStderr opts (Just srcdir) setupProgram
  where
    parmake_opt
        | Right ver <- ever,  ver >= Version [1,20] [] = ["-j"]
        | otherwise = []

    go :: ([String] -> IO ()) -> IO ()
    go run = do
      run $ [ "configure", "--package-db", db, "--prefix", db </> "prefix" ] ++ cabalOptions opts
      run $ [ "build" ] ++ parmake_opt
      run [ "copy" ]
      run [ "register" ]




newtype SetupProgram = SetupProgram { setupProgram :: FilePath }
compileSetupHs :: Options -> FilePath -> FilePath -> IO SetupProgram
compileSetupHs opts db srcdir = do
  ver <- ghcVersion opts
  let no_version_macros
        | ver >= Version [8] [] = [ "-fno-version-macros" ]
        | otherwise             = []

      file = srcdir </> "Setup"

  callProcessStderr opts (Just srcdir) (ghcProgram opts) $ concat
    [ [ "--make"
      , "-package-conf", db
      ]
    , no_version_macros
    , [ file <.> "hs"
      , "-o", file
      ]
    ]
  return $ SetupProgram file

data CabalPatchDescription = CabalPatchDescription {
      cpdVersions      :: [Version],
      cpdUnpackVariant :: UnpackCabalVariant,
      cpdPatchFn       :: FilePath -> IO ()
    }
nopCabalPatchDescription :: CabalPatchDescription
nopCabalPatchDescription = CabalPatchDescription [] LatestRevision (const (return ()))

patchyCabalVersions :: [CabalPatchDescription]
patchyCabalVersions = [
  let versions  = [ Version [1,18,1] [] ]
      variant   = Pristine
      patch     = fixArrayConstraint
  in CabalPatchDescription versions variant patch,

  let versions  = [ Version [1,18,0] [] ]
      variant   = Pristine
      patch dir = do
        fixArrayConstraint dir
        fixOrphanInstance dir
  in CabalPatchDescription versions variant patch,

  let versions  = [ Version [1,24,1,0] [] ]
      variant   = Pristine
      patch _   = return ()
  in CabalPatchDescription versions variant patch
  ]
 where
   fixArrayConstraint dir = do
     let cabalFile    = dir </> "Cabal.cabal"
         cabalFileTmp = cabalFile ++ ".tmp"

     cf <- readFile cabalFile
     writeFile cabalFileTmp $ replace "&& < 0.5" "&& < 0.6" cf
     renameFile cabalFileTmp cabalFile

   fixOrphanInstance dir = do
     let versionFile    = dir </> "Distribution/Version.hs"
         versionFileTmp = versionFile ++ ".tmp"

     let languagePragma =
           "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}"
         languagePragmaCPP =
           "{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving #-}"

         derivingDataVersion =
           "deriving instance Data Version"
         derivingDataVersionCPP = unlines [
             "#if __GLASGOW_HASKELL__ < 707",
             derivingDataVersion,
             "#endif"
           ]

     vf <- readFile versionFile
     writeFile versionFileTmp
       $ replace derivingDataVersion derivingDataVersionCPP
       $ replace languagePragma languagePragmaCPP vf

     renameFile versionFileTmp versionFile

unpackPatchedCabal
    :: Options
    -> Version
    -> FilePath
    -> CabalPatchDescription
    -> IO CabalSourceDir
unpackPatchedCabal opts cabalVer tmpdir (CabalPatchDescription _ variant patch) = do
  res@(CabalSourceDir dir) <- unpackCabal opts cabalVer tmpdir variant
  patch dir
  return res

data UnpackCabalVariant = Pristine | LatestRevision
newtype CabalSourceDir = CabalSourceDir { cabalSourceDir :: FilePath }
unpackCabal
    :: Options -> Version -> FilePath -> UnpackCabalVariant -> IO CabalSourceDir
unpackCabal opts cabalVer tmpdir variant = do
  let cabal = "Cabal-" ++ showVersion cabalVer
      dir = tmpdir </> cabal
      variant_opts = case variant of Pristine -> [ "--pristine" ]; _ -> []
      args = [ "get", cabal ] ++ variant_opts
  callProcessStderr opts (Just tmpdir) (cabalProgram opts) args
  return $ CabalSourceDir dir

unpackCabalHEAD :: FilePath -> IO (CabalSourceDir, String)
unpackCabalHEAD tmpdir = do
  let dir = tmpdir </> "cabal-head.git"
      url = "https://github.com/haskell/cabal.git"
  ExitSuccess <- rawSystem "git" [ "clone", "--depth=1", url, dir]
  commit <-
      withDirectory_ dir $ trim <$> readProcess "git" ["rev-parse", "HEAD"] ""
  return (CabalSourceDir $ dir </> "Cabal", commit)
 where
   withDirectory_ :: FilePath -> IO a -> IO a
   withDirectory_ dir action =
       bracket
         (liftIO getCurrentDirectory)
         (liftIO . setCurrentDirectory)
         (\_ -> liftIO (setCurrentDirectory dir) >> action)

errorInstallCabal :: Version -> FilePath -> IO a
errorInstallCabal cabalVer _distdir = panicIO $ printf "\
\Installing Cabal version %s failed.\n\
\\n\
\You have the following choices to fix this:\n\
\\n\
\- The easiest way to try and fix this is just reconfigure the project and try\n\
\  again:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If that fails you can try to install the version of Cabal mentioned above\n\
\  into your global/user package-db somehow, you'll probably have to fix\n\
\  something otherwise it wouldn't have failed above:\n\
\        $ cabal install Cabal --constraint 'Cabal == %s'\n\
\\n\
\- If you're using `Build-Type: Simple`:\n\
\  - You can see if you can reinstall your cabal-install executable while\n\
\    having it linked to a version of Cabal that's available in you\n\
\    package-dbs or can be built automatically:\n\
\        $ ghc-pkg list | grep Cabal  # find an available Cabal version\n\
\            Cabal-W.X.Y.Z\n\
\        $ cabal install cabal-install --constraint 'Cabal == W.X.*'\n\
\    Afterwards you'll have to reconfigure your project:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If you're using `Build-Type: Custom`:\n\
\  - Have cabal-install rebuild your Setup.hs executable with a version of the\n\
\    Cabal library that you have available in your global/user package-db:\n\
\        $ cabal clean && cabal configure\n\
\    You might also have to install some version of the Cabal to do this:\n\
\        $ cabal install Cabal\n\
\\n" sver sver
 where
   sver = showVersion cabalVer

cachedExe :: Version -> IO (Maybe FilePath)
cachedExe compCabalVersion = do
    appdir <- appCacheDir
    let exe = appdir </> exeName (Right compCabalVersion)
    exists <- doesFileExist exe
    return $ if exists then Just exe else Nothing

listCabalVersions :: Options -> IO [Version]
listCabalVersions opts = listCabalVersions' opts Nothing

-- TODO: Include sandbox? Probably only relevant for build-type:custom projects.
listCabalVersions' :: Options -> Maybe PackageDbDir -> IO [Version]
listCabalVersions' Options {..} mdb = do
  let mdbopt = ("--package-conf="++) <$> packageDbDir <$> mdb
      opts = ["list", "--simple-output", "Cabal"] ++ maybeToList mdbopt

  catMaybes . map (fmap snd . parsePkgId . fromString) . words
          <$> readProcess ghcPkgProgram opts ""

cabalVersionExistsInPkgDb :: Options -> Version -> IO Bool
cabalVersionExistsInPkgDb opts cabalVer = do
  db@(PackageDbDir db_path) <- getPrivateCabalPkgDb opts (Right cabalVer)
  exists <- doesDirectoryExist db_path
  case exists of
    False -> return False
    True -> do
      vers <- listCabalVersions' opts (Just db)
      return $ cabalVer `elem` vers

ghcVersion :: Options -> IO Version
ghcVersion Options {..} = do
    parseVer . trim <$> readProcess ghcProgram ["--numeric-version"] ""

ghcPkgVersion :: Options -> IO Version
ghcPkgVersion Options {..} = do
    parseVer . trim . dropWhile (not . isDigit) <$> readProcess ghcPkgProgram ["--version"] ""

newtype CabalInstallVersion = CabalInstallVersion { cabalInstallVer :: Version }
cabalInstallVersion :: Options -> IO CabalInstallVersion
cabalInstallVersion Options {..} = do
    CabalInstallVersion . parseVer . trim
      <$> readProcess cabalProgram ["--numeric-version"] ""

createPkgDb :: Options -> Either String Version -> IO PackageDbDir
createPkgDb opts@Options {..} cabalVer = do
  db@(PackageDbDir db_path) <- getPrivateCabalPkgDb opts cabalVer
  exists <- doesDirectoryExist db_path
  when (not exists) $ callProcessStderr opts Nothing ghcPkgProgram ["init", db_path]
  return db

getPrivateCabalPkgDb :: Options -> Either String Version -> IO PackageDbDir
getPrivateCabalPkgDb opts cabalVer = do
  appdir <- appCacheDir
  ghcVer <- ghcVersion opts
  let db_path = appdir </> exeName cabalVer
                ++ "-ghc" ++ showVersion ghcVer
                ++ ".package-db"
  return $ PackageDbDir db_path

-- "Cabal" ++ ver ++ "-ghc" ++ showVersion ghcVer

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion cabalFile =
  fromJust $ parseVer . extract <$> find ("version:" `isPrefixOf`) ls
 where
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> drop 1 >>> dropWhile isSpace >>> takeWhile (not . isSpace)

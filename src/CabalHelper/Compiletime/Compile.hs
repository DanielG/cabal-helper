-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE FlexibleContexts, DeriveFunctor, GADTs, ConstraintKinds,
  ImplicitParams, NamedFieldPuns, RecordWildCards #-}

{-|
Module      : CabalHelper.Compiletime.Compile
Description : Runtime compilation machinery
License     : GPL-3
-}

module CabalHelper.Compiletime.Compile where

import qualified Cabal.Plan as CP
import Cabal.Plan
  ( PlanJson(..), PkgId(..), PkgName(..), Ver(..), uPId)
import Control.Applicative
import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.Version
import GHC.IO.Exception (IOErrorType(OtherError))
import Text.Printf
import Text.Read
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.Environment
import System.IO
import System.IO.Error
import System.IO.Temp
import Prelude

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import Distribution.System
  ( buildPlatform )
import Distribution.Text
  ( display )

import Paths_cabal_helper
  ( version )

--import CabalHelper.Compiletime.Cabal
import CabalHelper.Compiletime.Data
--import CabalHelper.Compiletime.Log
--import CabalHelper.Compiletime.Program.GHC
--import CabalHelper.Compiletime.Program.CabalInstall
import CabalHelper.Compiletime.Types

import CabalHelper.Shared.Common
import CabalHelper.Shared.Sandbox (getSandboxPkgDb)

data Compile
    = CompileWithCabalSource
      { compCabalSourceDir     :: CabalSourceDir
      , compCabalSourceVersion :: Version
      }
    | CompileWithCabalPackage
      { compPackageDb      :: Maybe PackageDbDir
      , compCabalVersion   :: CabalVersion
      , compPackageDeps    :: [String]
      , compProductTarget  :: CompilationProductScope
      }

data CompPaths = CompPaths
    { compBuildDir:: FilePath
    , compOutDir  :: FilePath
    , compExePath :: FilePath
    }

-- | The Helper executable we produce as a compilation product can either be
-- placed in a per-project location, or a per-user/global location in the user's
-- home directory. This type controls where the compilation process places the
-- executable.
data CompilationProductScope = CPSGlobal | CPSProject

data CompHelperEnv = CompHelperEnv
  { cheCabalVer :: Version
  , chePkgDb    :: Maybe PackageDbDir
  , cheProjDir  :: FilePath
  , cheNewstyle :: Maybe (PlanJson, FilePath)
  , cheCacheDir :: FilePath
  }

compileHelper :: Env => CompHelperEnv -> IO (Either ExitCode FilePath)
compileHelper CompHelperEnv{..}   = do
    ghcVer <- ghcVersion
    Just (prepare, comp) <- runMaybeT $ msum $
      case chePkgDb of
        Nothing ->
          [ compileCabalSource
          , compileNewBuild ghcVer
          , compileSandbox ghcVer
          , compileGlobal
          , MaybeT $ Just <$> compileWithCabalInPrivatePkgDb
          ]
        Just db ->
          [ pure $ (pure (), compileWithPkg (Just db) cheCabalVer CPSProject)
          ]

    appdir <- appCacheDir

    let cp@CompPaths {compExePath} = compPaths appdir cheCacheDir comp
    exists <- doesFileExist compExePath
    if exists
      then do
        vLog $ "helper already compiled, using exe: "++compExePath
        return (Right compExePath)
      else do
        vLog $ "helper exe does not exist, compiling "++compExePath
        prepare >> compile comp cp

  where
   logMsg = "using helper compiled with Cabal from "

-- for relaxed deps: find (sameMajorVersionAs cheCabalVer) . reverse . sort

   -- | Check if this version is globally available
   compileGlobal :: Env => MaybeT IO (IO (), Compile)
   compileGlobal = do
       cabal_versions <- listCabalVersions' Nothing
       ver <- MaybeT $ return $ find (== cheCabalVer) cabal_versions
       vLog $ logMsg ++ "user/global package-db"
       return $ (return (), compileWithPkg Nothing ver CPSGlobal)

   -- | Check if this version is available in the project sandbox
   compileSandbox :: Env => Version -> MaybeT IO (IO (), Compile)
   compileSandbox ghcVer = do
       let mdb_path = getSandboxPkgDb (display buildPlatform) ghcVer cheProjDir
       sandbox <- PackageDbDir <$> MaybeT mdb_path
       cabal_versions <- listCabalVersions' (Just sandbox)
       ver <- MaybeT $ return $ find (== cheCabalVer) cabal_versions
       vLog $ logMsg ++ "sandbox package-db"
       return $ (return (), compileWithPkg (Just sandbox) ver CPSProject)

   compileNewBuild :: Env => Version -> MaybeT IO (IO (), Compile)
   compileNewBuild ghcVer = do
       (PlanJson {pjUnits}, distdir_newstyle) <- maybe mzero pure cheNewstyle
       let cabal_pkgid =
               PkgId (PkgName (Text.pack "Cabal"))
                        (Ver $ versionBranch cheCabalVer)
           mcabal_unit = listToMaybe $
             Map.elems $ Map.filter (\CP.Unit{..} -> uPId == cabal_pkgid) pjUnits
       CP.Unit {} <- maybe mzero pure mcabal_unit
       let inplace_db_path = distdir_newstyle
             </> "packagedb" </> ("ghc-" ++ showVersion ghcVer)
           inplace_db = PackageDbDir inplace_db_path
       cabal_versions <- listCabalVersions' (Just inplace_db)
       ver <- MaybeT $ return $ find (== cheCabalVer) cabal_versions
       vLog $ logMsg ++ "v2-build package-db " ++ inplace_db_path
       return $ (return (), compileWithPkg (Just inplace_db) ver CPSProject)

   -- | Compile the requested Cabal version into an isolated package-db if it's
   -- not there already
   compileWithCabalInPrivatePkgDb :: Env => IO (IO (), Compile)
   compileWithCabalInPrivatePkgDb = do
       db@(PackageDbDir db_path)
           <- getPrivateCabalPkgDb (CabalVersion cheCabalVer)
       vLog $ logMsg ++ "private package-db in " ++ db_path
       return (prepare db, compileWithPkg (Just db) cheCabalVer CPSGlobal)
     where
       prepare db = do
         db_exists <- liftIO $ cabalVersionExistsInPkgDb cheCabalVer db
         when (not db_exists) $
           void $ installCabal (Right cheCabalVer) `E.catch`
             \(SomeException _) -> errorInstallCabal cheCabalVer

   -- | See if we're in a cabal source tree
   compileCabalSource :: Env => MaybeT IO (IO (), Compile)
   compileCabalSource = do
       let cabalFile = cheProjDir </> "Cabal.cabal"
       cabalSrc <- liftIO $ doesFileExist cabalFile
       let projdir = CabalSourceDir cheProjDir
       case cabalSrc of
         False -> mzero
         True -> do
           vLog $ "projdir looks like Cabal source tree (Cabal.cabal exists)"
           cf <- liftIO $ readFile cabalFile
           let buildType = cabalFileBuildType cf
               ver       = cabalFileVersion cf

           case buildType of
             "simple" -> do
                 vLog $ "Cabal source tree is build-type:simple, moving on"
                 mzero
             "custom" -> do
                 vLog $ "compiling helper with local Cabal source tree"
                 return $ (return (), compileWithCabalSource projdir ver)
             _ -> error $ "compileCabalSource: unknown build-type: '"++buildType++"'"

   compileWithCabalSource srcDir ver =
       CompileWithCabalSource
          { compCabalSourceDir       = srcDir
          , compCabalSourceVersion   = ver
          }

   compileWithPkg mdb ver target =
       CompileWithCabalPackage
          { compPackageDb            = mdb
          , compCabalVersion         = CabalVersion ver
          , compPackageDeps          = [cabalPkgId ver]
          , compProductTarget        = target
          }

   cabalPkgId v = "Cabal-" ++ showVersion v

compile :: Env => Compile -> CompPaths -> IO (Either ExitCode FilePath)
compile comp paths@CompPaths {..} = do
    createDirectoryIfMissing True compOutDir
    createHelperSources compBuildDir

    vLog $ "compBuildDir: " ++ compBuildDir
    vLog $ "compOutDir: " ++ compOutDir
    vLog $ "compExePath: " ++ compExePath

    invokeGhc $ compGhcInvocation comp paths

compPaths :: FilePath -> FilePath -> Compile -> CompPaths
compPaths appdir cachedir c =
    case c of
      CompileWithCabalPackage {compProductTarget=CPSGlobal,..} -> CompPaths {..}
        where
          compBuildDir = appdir </> sourceHash <.> "build"
          compOutDir  = compBuildDir
          compExePath = compBuildDir </> "cabal-helper"

      CompileWithCabalPackage {compProductTarget=CPSProject,..} -> cachedirPaths
      CompileWithCabalSource {..} -> cachedirPaths
  where
    cachedirPaths = CompPaths {..}
        where
          compBuildDir = cachedir </> "cabal-helper"
          compOutDir  = compBuildDir
          compExePath = compOutDir </> "cabal-helper"

data GhcInvocation = GhcInvocation
    { giOutDir          :: FilePath
    , giOutput          :: FilePath
    , giCPPOptions      :: [String]
    , giPackageDBs      :: [PackageDbDir]
    , giIncludeDirs     :: [FilePath]
    , giHideAllPackages :: Bool
    , giPackages        :: [String]
    , giWarningFlags    :: [String]
    , giInputs          :: [String]
    }

compGhcInvocation :: Compile -> CompPaths -> GhcInvocation
compGhcInvocation comp CompPaths {..} =
    case comp of
      CompileWithCabalSource {..} ->
        GhcInvocation
          { giIncludeDirs = [compBuildDir, unCabalSourceDir compCabalSourceDir]
          , giPackageDBs  = []
          , giHideAllPackages = False
          , giPackages    = []
          , giCPPOptions = cppOptions compCabalSourceVersion
                           ++ [cabalVersionMacro compCabalSourceVersion]
          , ..
          }
      CompileWithCabalPackage {..} ->
        GhcInvocation
          { giIncludeDirs = [compBuildDir]
          , giPackageDBs = maybeToList compPackageDb
          , giHideAllPackages = True
          , giPackages =
              [ "base"
              , "containers"
              , "directory"
              , "filepath"
              , "process"
              , "bytestring"
              , "ghc-prim"
              ] ++ compPackageDeps
          , giCPPOptions = cppOptions (unCabalVersion compCabalVersion)
          , ..
          }
  where

    unCabalVersion (CabalVersion ver) = ver
    unCabalVersion (CabalHEAD _)      = Version [10000000, 0, 0] []

    cppOptions cabalVer =
        [ "-DCABAL_HELPER=1"
        , cabalMinVersionMacro cabalVer
        ]

    giOutDir = compOutDir
    giOutput = compExePath
    giWarningFlags = [ "-w" ] -- no point in bothering end users with warnings
    giInputs = [compBuildDir</>"CabalHelper"</>"Runtime"</>"Main.hs"]

cabalVersionMacro :: Version -> String
cabalVersionMacro (Version vs _) =
  "-DCABAL_VERSION="++intercalate "," (map show vs)

cabalMinVersionMacro :: Version -> String
cabalMinVersionMacro (Version (mj1:mj2:mi:_) _) =
  "-DCH_MIN_VERSION_Cabal(major1,major2,minor)=\
  \(  (major1)  < "++show mj1++" \
  \|| (major1) == "++show mj1++" && (major2)  < "++show mj2++" \
  \|| (major1) == "++show mj1++" && (major2) == "++show mj2++" && (minor) <= "++show mi++
  ")"
cabalMinVersionMacro _ =
    error "cabalMinVersionMacro: Version must have at least 3 components"

invokeGhc
    :: (Verbose, CProgs) => GhcInvocation -> IO (Either ExitCode FilePath)
invokeGhc GhcInvocation {..} = do
    rv <- callProcessStderr' Nothing (ghcProgram ?cprogs) $ concat
      [ [ "-outputdir", giOutDir
        , "-o", giOutput
        ]
      , map ("-optP"++) giCPPOptions
      , map ("-package-conf="++) $ unPackageDbDir <$> giPackageDBs
      , map ("-i"++) $ nub $ "" : giIncludeDirs
      , if giHideAllPackages then ["-hide-all-packages"] else []
      , concatMap (\p -> ["-package", p]) giPackages
      , giWarningFlags
      , ["--make"]
      , giInputs
      ]
    return $
      case rv of
        ExitSuccess -> Right giOutput
        e@(ExitFailure _) -> Left e


-- | Cabal library version we're compiling the helper exe against.
data CabalVersion
    = CabalHEAD  { cvCommitId   :: CommitId }
    | CabalVersion { cabalVersion :: Version }

newtype CommitId = CommitId { unCommitId :: String }

exeName :: CabalVersion -> String
exeName (CabalHEAD commitid) = intercalate "-"
    [ "cabal-helper" ++ showVersion version
    , "CabalHEAD" ++ unCommitId commitid
    ]
exeName CabalVersion {cabalVersion} = intercalate "-"
    [ "cabal-helper" ++ showVersion version
    , "Cabal" ++ showVersion cabalVersion
    ]

readProcess' :: Verbose => FilePath -> [String] -> String -> IO String
readProcess' exe args inp = do
  vLog $ intercalate " " $ map formatProcessArg (exe:args)
  outp <- readProcess exe args inp
  vLog $ unlines $ map ("=> "++) $ lines outp
  return outp

callProcessStderr'
    :: Verbose => Maybe FilePath -> FilePath -> [String] -> IO ExitCode
callProcessStderr' mwd exe args = do
  let cd = case mwd of
             Nothing -> []; Just wd -> [ "cd", formatProcessArg wd++";" ]
  vLog $ intercalate " " $ cd ++ map formatProcessArg (exe:args)
  (_, _, _, h) <- createProcess (proc exe args) { std_out = UseHandle stderr
                                                , cwd = mwd }
  waitForProcess h

callProcessStderr :: Verbose => Maybe FilePath -> FilePath -> [String] -> IO ()
callProcessStderr mwd exe args = do
  rv <- callProcessStderr' mwd exe args
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

installCabal :: Env => Either HEAD Version -> IO (PackageDbDir, CabalVersion)
installCabal ever = do
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
    (srcdir, cabalVer) <- case ever of
      Left HEAD -> do
        second CabalHEAD <$> unpackCabalHEAD tmpdir
      Right ver -> do
        message ver
        let patch = fromMaybe nopCabalPatchDescription $
              find ((ver`elem`) . cpdVersions) patchyCabalVersions
        (,) <$> unpackPatchedCabal ver tmpdir patch <*> pure (CabalVersion ver)

    db <- createPkgDb cabalVer

    runCabalInstall db srcdir ever

    return (db, cabalVer)

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
    :: Env => PackageDbDir -> CabalSourceDir -> Either HEAD Version-> IO ()
runCabalInstall (PackageDbDir db) (CabalSourceDir srcdir) ever = do
  civ@CabalInstallVersion {..} <- cabalInstallVersion
  cabal_opts <- return $ concat
      [
        [ "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--prefix=" ++ db </> "prefix"
        ]
        , withGHCProgramOptions
        , if cabalInstallVer >= Version [1,20,0,0] []
             then ["--no-require-sandbox"]
             else []
        , [ "install", srcdir ]
        , if ?verbose
            then ["-v"]
            else []
        , [ "--only-dependencies" ]
      ]

  callProcessStderr (Just "/") oCabalProgram cabal_opts

  runSetupHs db srcdir ever civ

  hPutStrLn stderr "done"

withGHCProgramOptions :: Env => [String]
withGHCProgramOptions =
    concat [ [ "--with-ghc=" ++ ghcProgram ?cprogs ]
           , if ghcProgram ?cprogs /= ghcPkgProgram defaultCompPrograms
               then [ "--with-ghc-pkg=" ++ ghcPkgProgram ?cprogs ]
               else []
           ]

runSetupHs
    :: Env
    => FilePath
    -> FilePath
    -> Either HEAD Version
    -> CabalInstallVersion
    -> IO ()
runSetupHs db srcdir ever CabalInstallVersion {..}
    | cabalInstallVer >= parseVer "1.24" = do
      go $ \args -> callProcessStderr (Just srcdir) oCabalProgram $
        [ "act-as-setup", "--" ] ++ args
    | otherwise = do
      SetupProgram {..} <- compileSetupHs db srcdir
      go $ callProcessStderr (Just srcdir) setupProgram
  where
    parmake_opt :: Maybe Int -> [String]
    parmake_opt nproc'
        | Left _ <- ever = ["-j"++nproc]
        | Right ver <- ever,  ver >= Version [1,20] [] = ["-j"++nproc]
        | otherwise = []
      where
        nproc = fromMaybe "" $ show <$> nproc'

    go :: ([String] -> IO ()) -> IO ()
    go run = do
      run $ [ "configure", "--package-db", db, "--prefix", db </> "prefix" ]
              ++ withGHCProgramOptions
      mnproc <- join . fmap readMaybe <$> lookupEnv "NPROC"
      run $ [ "build" ] ++ parmake_opt mnproc
      run [ "copy" ]
      run [ "register" ]




newtype SetupProgram = SetupProgram { setupProgram :: FilePath }
compileSetupHs :: Env => FilePath -> FilePath -> IO SetupProgram
compileSetupHs db srcdir = do
  ver <- ghcVersion
  let no_version_macros
        | ver >= Version [8] [] = [ "-fno-version-macros" ]
        | otherwise             = []

      file = srcdir </> "Setup"

  callProcessStderr (Just srcdir) (ghcProgram ?cprogs) $ concat
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
    :: Env
    => Version
    -> FilePath
    -> CabalPatchDescription
    -> IO CabalSourceDir
unpackPatchedCabal cabalVer tmpdir (CabalPatchDescription _ variant patch) = do
  res@(CabalSourceDir dir) <- unpackCabal cabalVer tmpdir variant
  patch dir
  return res

data UnpackCabalVariant = Pristine | LatestRevision
newtype CabalSourceDir = CabalSourceDir { unCabalSourceDir :: FilePath }
unpackCabal
    :: Env => Version -> FilePath -> UnpackCabalVariant -> IO CabalSourceDir
unpackCabal cabalVer tmpdir variant = do
  let cabal = "Cabal-" ++ showVersion cabalVer
      dir = tmpdir </> cabal
      variant_opts = case variant of Pristine -> [ "--pristine" ]; _ -> []
      args = [ "get", cabal ] ++ variant_opts
  callProcessStderr (Just tmpdir) oCabalProgram args
  return $ CabalSourceDir dir

unpackCabalHEAD :: Env => FilePath -> IO (CabalSourceDir, CommitId)
unpackCabalHEAD tmpdir = do
  let dir = tmpdir </> "cabal-head.git"
      url = "https://github.com/haskell/cabal.git"
  ExitSuccess <- rawSystem "git" [ "clone", "--depth=1", url, dir]
  commit <-
      withDirectory_ dir $ trim <$> readProcess' "git" ["rev-parse", "HEAD"] ""
  return (CabalSourceDir $ dir </> "Cabal", CommitId commit)
 where
   withDirectory_ :: FilePath -> IO a -> IO a
   withDirectory_ dir action =
       bracket
         (liftIO getCurrentDirectory)
         (liftIO . setCurrentDirectory)
         (\_ -> liftIO (setCurrentDirectory dir) >> action)

errorInstallCabal :: Version -> IO a
errorInstallCabal cabalVer = panicIO $ printf "\
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

listCabalVersions' :: Env => Maybe PackageDbDir -> MaybeT IO [Version]
listCabalVersions' mdb = do
  case mdb of
    Nothing -> mzero
    Just (PackageDbDir db_path) -> do
      exists <- liftIO $ doesDirectoryExist db_path
      case exists of
        False -> mzero
        True  -> MaybeT $ logIOError "listCabalVersions'" $ Just <$> do
          let mdbopt = ("--package-conf="++) <$> unPackageDbDir <$> mdb
              args = ["list", "--simple-output", "Cabal"] ++ maybeToList mdbopt

          catMaybes . map (fmap snd . parsePkgId . fromString) . words
                   <$> readProcess' (ghcProgram ?cprogs) args ""

cabalVersionExistsInPkgDb :: Env => Version -> PackageDbDir -> IO Bool
cabalVersionExistsInPkgDb cabalVer db@(PackageDbDir db_path) = do
  exists <- doesDirectoryExist db_path
  case exists of
    False -> return False
    True -> fromMaybe False <$> runMaybeT (do
      vers <- listCabalVersions' (Just db)
      return $ cabalVer `elem` vers)

ghcVersion :: (Verbose, CProgs) => IO Version
ghcVersion = do
  parseVer . trim <$> readProcess' (ghcProgram ?cprogs) ["--numeric-version"] ""

ghcPkgVersion :: (Verbose, CProgs) => IO Version
ghcPkgVersion =
  parseVer . trim . dropWhile (not . isDigit)
    <$> readProcess' (ghcPkgProgram ?cprogs) ["--version"] ""

newtype CabalInstallVersion = CabalInstallVersion { cabalInstallVer :: Version }
cabalInstallVersion :: Env => IO CabalInstallVersion
cabalInstallVersion = do
  CabalInstallVersion . parseVer . trim
    <$> readProcess' oCabalProgram ["--numeric-version"] ""

createPkgDb :: (Verbose, CProgs) => CabalVersion -> IO PackageDbDir
createPkgDb cabalVer = do
  db@(PackageDbDir db_path) <- getPrivateCabalPkgDb cabalVer
  exists <- doesDirectoryExist db_path
  when (not exists) $
       callProcessStderr Nothing (ghcPkgProgram ?cprogs) ["init", db_path]
  return db

getPrivateCabalPkgDb :: (Verbose, CProgs) => CabalVersion -> IO PackageDbDir
getPrivateCabalPkgDb cabalVer = do
  appdir <- appCacheDir
  ghcVer <- ghcVersion
  let db_path = appdir </> exeName cabalVer
                ++ "-ghc" ++ showVersion ghcVer
                ++ ".package-db"
  return $ PackageDbDir db_path

-- "Cabal" ++ ver ++ "-ghc" ++ showVersion ghcVer

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion = parseVer . cabalFileTopField "version"

-- | Find @build-type: XXX@ delcaration in a cabal file
cabalFileBuildType :: String -> String
cabalFileBuildType = cabalFileTopField "build-type"

cabalFileTopField :: String -> String -> String
cabalFileTopField field cabalFile = value
 where
  Just value = extract <$> find ((field++":") `isPrefixOf`) ls
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> drop 1 >>> dropWhile isSpace >>> takeWhile (not . isSpace)

vLog :: (MonadIO m, Verbose) => String -> m ()
vLog msg
    | ?verbose  = liftIO $ hPutStrLn stderr msg
    | otherwise = return ()

logIOError :: Verbose => String -> IO (Maybe a) -> IO (Maybe a)
logIOError label a = do
  a `catchIOError` \ex -> do
      vLog $ label ++ ": " ++ show ex
      return Nothing

-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
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
module Main where

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
import Text.Printf
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import Prelude

import Distribution.System (buildPlatform)
import Distribution.Text (display)

import Paths_cabal_helper (version)
import CabalHelper.Data
import CabalHelper.Common
import CabalHelper.GuessGhc

usage :: IO ()
usage = do
  prog <- getProgName
  hPutStr stderr $ align "(" "|" ("Usage: " ++ prog ++ " " ++ usageMsg)
 where
   usageMsg = "\
\( print-appdatadir\n\
\| print-build-platform\n\
\| [--verbose]\n\
\  [--with-ghc=GHC_PATH]\n\
\  [--with-ghc-pkg=GHC_PKG_PATH]\n\
\  [--with-cabal=CABAL_PATH]\n\
\  DIST_DIR ( print-exe | [CABAL_HELPER_ARGS...] ) )\n"

data Options = Options {
          verbose       :: Bool
        , ghcProgram    :: FilePath
        , ghcPkgProgram :: FilePath
        , cabalProgram  :: FilePath
}

defaultOptions :: Options
defaultOptions = Options False "ghc" "ghc-pkg" "cabal"

globalArgSpec :: [OptDescr (Options -> Options)]
globalArgSpec =
      [ option "" ["verbose"] "Be more verbose" $
              NoArg $ \o -> o { verbose = True }

      , option "" ["with-ghc"] "GHC executable to use" $
              reqArg "PROG" $ \p o -> o { ghcProgram = p }

      , option "" ["with-ghc-pkg"] "ghc-pkg executable to use (only needed when guessing from GHC path fails)" $
              reqArg "PROG" $ \p o -> o { ghcPkgProgram = p }

      , option "" ["with-cabal"] "cabal-install executable to use" $
               reqArg "PROG" $ \p o -> o { cabalProgram = p }
      ]
 where
   option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
   option s l udsc dsc = Option s l dsc udsc

   reqArg :: String -> (String -> a) -> ArgDescr a
   reqArg udsc dsc = ReqArg dsc udsc

parseCommandArgs :: Options -> [String] -> (Options, [String])
parseCommandArgs opts argv
    = case getOpt RequireOrder globalArgSpec argv of
        (o,r,[])   -> (foldr id opts o, r)
        (_,_,errs) ->
            panic $ "Parsing command options failed:\n" ++ concat errs

guessProgramPaths :: Options -> IO Options
guessProgramPaths opts = do
    if not (same ghcProgram opts dopts) && same ghcPkgProgram opts dopts
       then do
         mghcPkg <- guessToolFromGhcPath "ghc-pkg" (ghcProgram opts)
         return opts {
           ghcPkgProgram = fromMaybe (ghcPkgProgram opts) mghcPkg
         }
       else return opts
 where
   same f o o'  = f o == f o'
   dopts = defaultOptions

main :: IO ()
main = handlePanic $ do
  (opts', args) <- parseCommandArgs defaultOptions <$> getArgs
  opts <- guessProgramPaths opts'
  case args of
    [] -> usage
    "help":[] -> usage
    "version":[] -> putStrLn $ showVersion version
    "print-appdatadir":[] -> putStrLn =<< appDataDir
    "print-build-platform":[] -> putStrLn $ display buildPlatform
    distdir:args' -> do
      cfgf <- canonicalizePath (distdir </> "setup-config")
      mhdr <- getCabalConfigHeader cfgf
      case mhdr of
        Nothing -> panic $ printf "\
\Could not read Cabal's persistent setup configuration header\n\
\- Check first line of: %s\n\
\- Maybe try: $ cabal configure" cfgf
        Just (hdrCabalVersion, _) -> do
          eexe <- compileHelper opts hdrCabalVersion distdir
          case eexe of
              Left e -> exitWith e
              Right exe ->
                case args' of
                  "print-exe":_ -> putStrLn exe
                  _ -> do
                    (_,_,_,h) <- createProcess $ proc exe args
                    exitWith =<< waitForProcess h

appDataDir :: IO FilePath
appDataDir = (</> "cabal-helper") <$> getAppUserDataDirectory "ghc-mod"

compileHelper :: Options -> Version -> FilePath -> IO (Either ExitCode FilePath)
compileHelper opts cabalVer distdir = withHelperSources $ \chdir -> do
  run [ compileCabalSource chdir -- TODO: here ghc's caching fails and it always
                                 -- recompiles, probably because we write the
                                 -- sources to a tempdir and they always look
                                 -- newer than the Cabal sources, not sure if we
                                 -- can fix this
      , Right <$> MaybeT (cachedExe cabalVer)
      , compileGlobal chdir
      , cachedCabalPkg chdir
      , MaybeT (Just <$> compileSandbox chdir)
      ]

 where
   run actions = fromJust <$> runMaybeT (msum actions)

   logMsg = "compiling helper with Cabal from "


-- for relaxed deps: find (sameMajorVersionAs cabalVer) . reverse . sort

   -- | Check if this version is globally available
   compileGlobal :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   compileGlobal chdir = do
      -- TODO: add option to let user specify custom package-db, relevant when
      -- using a Cabal compiled from git!

       ver <- MaybeT $ find (== cabalVer) <$> listCabalVersions opts
       vLog opts $ logMsg ++ "user/global package-db"
       liftIO $ compileWithPkg chdir Nothing ver

   -- | Check if we already compiled this version of cabal into a private
   -- package-db
   cachedCabalPkg :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   cachedCabalPkg chdir = do
       db_exists <- liftIO $ cabalPkgDbExists opts cabalVer
       case db_exists of
         False -> mzero
         True -> do
             db <- liftIO $ cabalPkgDb opts cabalVer
             vLog opts $ logMsg ++ "private package-db in " ++ db
             liftIO $ compileWithPkg chdir (Just db) cabalVer

   -- | See if we're in a cabal source tree
   compileCabalSource :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   compileCabalSource chdir = do
       let couldBeSrcDir = takeDirectory distdir
           cabalFile = couldBeSrcDir </> "Cabal.cabal"
           isCabalMagicVer = cabalVer == Version [1,9999] []
       cabalSrc <- liftIO $ doesFileExist cabalFile

       when isCabalMagicVer $
         vLog opts $ "cabal magic version (1.9999) found"

       when cabalSrc $
         vLog opts $ "directory above distdir looks like cabal source tree (Cabal.cabal exists)"

       case isCabalMagicVer || cabalSrc of
         False -> mzero
         True -> liftIO $ do
           ver <- cabalFileVersion <$> readFile cabalFile
           vLog opts $ "compiling helper with local Cabal source tree"
           compileWithCabalTree chdir ver couldBeSrcDir

   -- | Compile the requested cabal version into an isolated package-db
   compileSandbox :: FilePath -> IO (Either ExitCode FilePath)
   compileSandbox chdir = do
       db <- installCabal opts cabalVer `E.catch`
             \(SomeException _) -> errorInstallCabal cabalVer distdir
       compileWithPkg chdir (Just db) cabalVer

   compileWithCabalTree chdir ver srcDir =
       compile distdir opts $ Compile chdir (Just srcDir) Nothing ver []

   compileWithPkg chdir mdb ver =
       compile distdir opts $ Compile chdir Nothing mdb ver [cabalPkgId ver]

   cabalPkgId v = "Cabal-" ++ showVersion v

errorInstallCabal :: Version -> FilePath -> a
errorInstallCabal cabalVer _distdir = panic $ printf "\
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


data Compile = Compile {
      cabalHelperSourceDir :: FilePath,
      cabalSourceDir :: Maybe FilePath,
      packageDb      :: Maybe FilePath,
      cabalVersion   :: Version,
      packageDeps    :: [String]
    }

compile :: FilePath -> Options -> Compile -> IO (Either ExitCode FilePath)
compile distdir opts@Options {..} Compile {..} = do
    cCabalSourceDir <- canonicalizePath `traverse` cabalSourceDir
    appdir <- appDataDir

    let outdir' = maybe appdir (const $ distdir </> "cabal-helper") cCabalSourceDir
    createDirectoryIfMissing True outdir'
    outdir <- canonicalizePath outdir'

    let exedir' = maybe outdir (const distdir) cCabalSourceDir
    createDirectoryIfMissing True exedir'
    exedir <- canonicalizePath exedir'
    exe <- exePath' cabalVersion <$> canonicalizePath exedir

    vLog opts $ "outdir: " ++ outdir
    vLog opts $ "exedir: " ++ exedir

    let Version (mj:mi:_) _ = cabalVersion
    let ghc_opts =
             concat [
          [ "-outputdir", outdir
          , "-o", exe
          , "-optP-DCABAL_HELPER=1"
          , "-optP-DCABAL_MAJOR=" ++ show mj
          , "-optP-DCABAL_MINOR=" ++ show mi
          ],
          maybeToList $ ("-package-conf="++) <$> packageDb,
          map ("-i"++) $ nub $ ".":maybeToList cCabalSourceDir,

          if isNothing cCabalSourceDir
             then [ "-hide-all-packages"
                  , "-package", "base"
                  , "-package", "directory"
                  , "-package", "filepath"
                  , "-package", "process"
                  , "-package", "bytestring"
                  , "-package", "ghc-prim"
                  ]
             else [],

          concatMap (\p -> ["-package", p]) packageDeps,
          [ "--make",  "CabalHelper/Main.hs" ]
         ]

    vLog opts $ intercalate " " $ map (("\""++) . (++"\"")) $ ghcProgram:ghc_opts

    -- TODO: touch exe after, ghc doesn't do that if the input files didn't
    -- actually change
    rv <- callProcessStderr' (Just cabalHelperSourceDir) ghcProgram ghc_opts
    return $ case rv of
               ExitSuccess -> Right exe
               e@(ExitFailure _) -> Left e

exePath :: Version -> IO FilePath
exePath cabalVersion = do
    exePath' cabalVersion <$> appDataDir

exePath' :: Version-> FilePath -> FilePath
exePath' cabalVersion outdir =
    outdir </> "cabal-helper-" ++ showVersion version -- our ver
            ++ "-Cabal-" ++ showVersion cabalVersion

cachedExe :: Version -> IO (Maybe FilePath)
cachedExe cabalVersion = do
   exe <- exePath cabalVersion
   exists <- doesFileExist exe
   return $ if exists then Just exe else Nothing

callProcessStderr' :: Maybe FilePath -> FilePath -> [String] -> IO ExitCode
callProcessStderr' mwd exe args = do
  (_, _, _, h) <- createProcess (proc exe args) { std_out = UseHandle stderr
                                                , cwd = mwd }
  waitForProcess h

callProcessStderr :: Maybe FilePath -> FilePath -> [String] -> IO ()
callProcessStderr mwd exe args = do
  rv <- callProcessStderr' mwd exe args
  case rv of
    ExitSuccess -> return ()
    ExitFailure v -> processFailedException "callProcessStderr" exe args v

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fn exe args rv =
      panic $ concat [fn, ": ", exe, " "
                     , intercalate " " (map show args)
                     , " (exit " ++ show rv ++ ")"]

installCabal :: Options -> Version -> IO FilePath
installCabal opts ver = do
  appdir <- appDataDir
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
\Building Cabal %s ...\n" appdir sver sver sver

  db <- createPkgDb opts ver
  cabal_opts <- return $ concat
      [
        [ "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--prefix=" ++ db </> "prefix"
        , "--with-ghc=" ++ ghcProgram opts
        ]
        , if ghcPkgProgram opts /= ghcPkgProgram defaultOptions
            then [ "--with-ghc-pkg=" ++ ghcPkgProgram opts ]
            else []
        , [ "install", "Cabal", "--constraint"
          , "Cabal == " ++ showVersion ver ]
      ]

  vLog opts $ intercalate " " $ map (("\""++) . (++"\"")) $ cabalProgram opts:cabal_opts

  callProcessStderr (Just "/") (cabalProgram opts) cabal_opts
  hPutStrLn stderr "done"
  return db

ghcVersion :: Options -> IO Version
ghcVersion Options {..} = do
    parseVer . trim <$> readProcess ghcProgram ["--numeric-version"] ""

ghcPkgVersion :: Options -> IO Version
ghcPkgVersion Options {..} = do
    parseVer . trim . dropWhile (not . isDigit) <$> readProcess ghcPkgProgram ["--version"] ""

trim :: String -> String
trim = dropWhileEnd isSpace

createPkgDb :: Options -> Version -> IO FilePath
createPkgDb opts@Options {..} ver = do
  db <- cabalPkgDb opts ver
  exists <- doesDirectoryExist db
  when (not exists) $ callProcessStderr Nothing ghcPkgProgram ["init", db]
  return db

cabalPkgDb :: Options -> Version -> IO FilePath
cabalPkgDb opts ver = do
  appdir <- appDataDir
  ghcVer <- ghcVersion opts
  return $ appdir </> "Cabal-" ++ showVersion ver ++ "-db-" ++ showVersion ghcVer

cabalPkgDbExists :: Options -> Version -> IO Bool
cabalPkgDbExists opts ver = do
  db <- cabalPkgDb opts ver
  dexists <- doesDirectoryExist db
  case dexists of
    False -> return False
    True -> do
      vers <- listCabalVersions' opts (Just db)
      return $ ver `elem` vers

listCabalVersions :: Options -> IO [Version]
listCabalVersions opts = listCabalVersions' opts Nothing

-- TODO: Include sandbox? Probably only relevant for build-type:custom projects.
listCabalVersions' :: Options -> Maybe FilePath -> IO [Version]
listCabalVersions' Options {..} mdb = do
  let mdbopt = ("--package-conf="++) <$> mdb
      opts = ["list", "--simple-output", "Cabal"] ++ maybeToList mdbopt

  catMaybes . map (fmap snd . parsePkgId . fromString) . words
          <$> readProcess ghcPkgProgram opts ""

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion cabalFile =
  fromJust $ parseVer . extract <$> find ("version:" `isPrefixOf`) ls
 where
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> drop 1 >>> dropWhile isSpace >>> takeWhile (not . isSpace)

vLog :: MonadIO m => Options -> String -> m ()
vLog Options { verbose = True } msg =
    liftIO $ hPutStrLn stderr msg
vLog _ _ = return ()

-- ghc-mod: Making Haskell development *more* fun
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

{-# LANGUAGE CPP, FlexibleContexts, ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric #-}

module Distribution.Helper (
    Programs(..)

  -- * Running Queries
  , Query
  , runQuery
  , runQuery'
  , runQuery''

  -- * Queries against Cabal\'s on disk state

  , packageDbStack
  , entrypoints
  , sourceDirs
  , ghcOptions
  , ghcSrcOptions
  , ghcPkgOptions
  , ghcMergedPkgOptions
  , ghcLangOptions

  -- * Result types
  , ChModuleName(..)
  , ChComponentName(..)
  , ChPkgDb(..)
  , ChEntrypoint(..)

  -- * General information
  , buildPlatform

  -- * Stuff that cabal-install really should export
  , Distribution.Helper.getSandboxPkgDb

  -- * Managing @dist/@
  , prepare
  , reconfigure
  , writeAutogenFiles

  -- * $libexec related error handling
  , LibexecNotFoundError(..)
  , libexecNotFoundError
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception as E
import Data.Char
import Data.List
import Data.Default
import Data.Version
import Data.Typeable
import Distribution.Simple.BuildPaths (exeExtension)
import System.Environment
import System.FilePath
import System.Directory
import System.Process
import System.IO.Unsafe
import Text.Printf
import GHC.Generics
import Prelude

import Paths_cabal_helper (getLibexecDir)
import CabalHelper.Types hiding (Options(..))
import CabalHelper.Sandbox

-- | Paths or names of various programs we need.
data Programs = Programs {
      cabalProgram  :: FilePath,
      ghcProgram    :: FilePath,
      ghcPkgProgram :: FilePath
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Default Programs where
    def = Programs "cabal" "ghc" "ghc-pkg"

data SomeLocalBuildInfo = SomeLocalBuildInfo {
      slbiPackageDbStack      :: [ChPkgDb],
      slbiEntrypoints         :: [(ChComponentName, ChEntrypoint)],
      slbiSourceDirs          :: [(ChComponentName, [String])],
      slbiGhcOptions          :: [(ChComponentName, [String])],
      slbiGhcSrcOptions       :: [(ChComponentName, [String])],
      slbiGhcPkgOptions       :: [(ChComponentName, [String])],
      slbiGhcMergedPkgOptions :: [String],
      slbiGhcLangOptions      :: [(ChComponentName, [String])]
    } deriving (Eq, Ord, Read, Show)

-- | Caches helper executable result so it doesn't have to be run more than once
-- as reading in Cabal's @LocalBuildInfo@ datatype from disk is very slow but
-- running all possible queries against it at once is cheap.
newtype Query m a = Query { unQuery :: StateT (Maybe SomeLocalBuildInfo)
                                         (ReaderT QueryEnv m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

data QueryEnv = QueryEnv {
      _qeReadProcess :: FilePath -> [String] -> String -> IO String,
      _qeProgs       :: Programs,
      _qeProjectDir  :: FilePath,
      _qeDistDir     :: FilePath
    }

type MonadQuery m = ( MonadIO m
                    , MonadState (Maybe SomeLocalBuildInfo) m
                    , MonadReader QueryEnv m)

run :: Monad m
  => QueryEnv -> Maybe SomeLocalBuildInfo -> Query m a -> m a
run e s action = flip runReaderT e (flip evalStateT s (unQuery action))

-- | @runQuery query distdir@. Run a 'Query'. @distdir@ is where Cabal's
-- @setup-config@ file is located.
runQuery :: Monad m
         => FilePath -- ^ Path to project directory, i.e. the one containing the
                     -- @project.cabal@ file
         -> FilePath -- ^ Path to @dist/@
         -> Query m a
         -> m a
runQuery pd dd action = run (QueryEnv readProcess def pd dd) Nothing action

runQuery' :: Monad m
         => Programs
         -> FilePath -- ^ Path to project directory, i.e. the one containing the
                     -- @project.cabal@ file
         -> FilePath -- ^ Path to @dist/@
         -> Query m a
         -> m a
runQuery' progs pd dd action =
    run (QueryEnv readProcess progs pd dd) Nothing action

runQuery'' :: Monad m
         => (FilePath -> [String] -> String -> IO String)
         -- ^ How to start the cabal-helper process. Useful if you need to
         -- capture stderr output from the helper.
         -> Programs
         -> FilePath -- ^ Path to project directory, i.e. the one containing the
                     -- @project.cabal@ file
         -> FilePath -- ^ Path to @dist/@
         -> Query m a
         -> m a
runQuery'' readProc progs pd dd action =
    run (QueryEnv readProc progs pd dd) Nothing action


getSlbi :: MonadQuery m => m SomeLocalBuildInfo
getSlbi = do
  s <- get
  case s of
    Nothing -> do
            slbi <- getSomeConfigState
            put (Just slbi)
            return slbi
    Just slbi -> return slbi

-- | List of package databases to use.
packageDbStack :: MonadIO m => Query m [ChPkgDb]

-- | Modules or files Cabal would have the compiler build directly. Can be used
-- to compute the home module closure for a component.
entrypoints   :: MonadIO m => Query m [(ChComponentName, ChEntrypoint)]

-- | A component's @source-dirs@ field, beware as if this is empty implicit
-- behaviour in GHC kicks in.
sourceDirs    :: MonadIO m => Query m [(ChComponentName, [FilePath])]

-- | All options cabal would pass to GHC.
ghcOptions    :: MonadIO m => Query m [(ChComponentName, [String])]

-- | Only search path related GHC options.
ghcSrcOptions :: MonadIO m => Query m [(ChComponentName, [String])]

-- | Only package related GHC options, sufficient for things don't need to
-- access any home modules.
ghcPkgOptions :: MonadIO m => Query m [(ChComponentName, [String])]

-- | Like @ghcPkgOptions@ but for the whole package not just one component
ghcMergedPkgOptions :: MonadIO m => Query m [String]

-- | Only language related options, i.e. @-XSomeExtension@
ghcLangOptions :: MonadIO m => Query m [(ChComponentName, [String])]

packageDbStack      = Query $ slbiPackageDbStack      `liftM` getSlbi
entrypoints         = Query $ slbiEntrypoints         `liftM` getSlbi
sourceDirs          = Query $ slbiSourceDirs          `liftM` getSlbi
ghcOptions          = Query $ slbiGhcOptions          `liftM` getSlbi
ghcSrcOptions       = Query $ slbiGhcSrcOptions       `liftM` getSlbi
ghcPkgOptions       = Query $ slbiGhcPkgOptions       `liftM` getSlbi
ghcMergedPkgOptions = Query $ slbiGhcMergedPkgOptions `liftM` getSlbi
ghcLangOptions      = Query $ slbiGhcLangOptions      `liftM` getSlbi

-- | Run @cabal configure@
reconfigure :: MonadIO m
            => (FilePath -> [String] -> String -> IO String)
            -> Programs -- ^ Program paths
            -> [String] -- ^ Command line arguments to be passed to @cabal@
            -> m ()
reconfigure readProc progs cabalOpts = do
    let progOpts =
            [ "--with-ghc=" ++ ghcProgram progs ]
            -- Only pass ghc-pkg if it was actually set otherwise we
            -- might break cabal's guessing logic
            ++ if ghcPkgProgram progs /= ghcPkgProgram def
                 then [ "--with-ghc-pkg=" ++ ghcPkgProgram progs ]
                 else []
            ++ cabalOpts
    _ <- liftIO $ readProc (cabalProgram progs) ("configure":progOpts) ""
    return ()

getSomeConfigState :: MonadQuery m => m SomeLocalBuildInfo
getSomeConfigState = ask >>= \(QueryEnv readProc progs projdir distdir) -> do
  let progArgs = [ "--with-ghc="     ++ ghcProgram progs
                 , "--with-ghc-pkg=" ++ ghcPkgProgram progs
                 , "--with-cabal="   ++ cabalProgram progs
                 ]

  let args = [ "package-db-stack"
             , "entrypoints"
             , "source-dirs"
             , "ghc-options"
             , "ghc-src-options"
             , "ghc-pkg-options"
             , "ghc-merged-pkg-options"
             , "ghc-lang-options"
             ] ++ progArgs

  res <- liftIO $ do
    exe  <- findLibexecExe "cabal-helper-wrapper"
    out <- readProc exe (projdir:distdir:args) ""
    evaluate (read out) `E.catch` \(SomeException _) ->
      error $ concat ["getSomeConfigState", ": ", exe, " "
                     , intercalate " " (map show $ distdir:args)
                     , " (read failed)"]

  let [ Just (ChResponsePkgDbs pkgDbs),
        Just (ChResponseEntrypoints eps),
        Just (ChResponseCompList srcDirs),
        Just (ChResponseCompList ghcOpts),
        Just (ChResponseCompList ghcSrcOpts),
        Just (ChResponseCompList ghcPkgOpts),
        Just (ChResponseList     ghcMergedPkgOpts),
        Just (ChResponseCompList ghcLangOpts) ] = res

  return $ SomeLocalBuildInfo
    pkgDbs eps srcDirs ghcOpts ghcSrcOpts ghcPkgOpts ghcMergedPkgOpts ghcLangOpts

-- | Make sure the appropriate helper executable for the given project is
-- installed and ready to run queries.
prepare :: MonadIO m
        => (FilePath -> [String] -> String -> IO String)
        -> FilePath
        -- ^ Path to project directory, i.e. the one containing the
        -- @project.cabal@ file
        -> FilePath
        -- ^ Path to the @dist/@ directory
        -> m ()
prepare readProc projdir distdir = liftIO $ do
  exe  <- findLibexecExe "cabal-helper-wrapper"
  void $ readProc exe [projdir, distdir] ""

-- | Create @cabal_macros.h@ and @Paths_\<pkg\>@ possibly other generated files
-- in the usual place.
writeAutogenFiles :: MonadIO m
                  => (FilePath -> [String] -> String -> IO String)
                  -> FilePath
                  -- ^ Path to project directory, i.e. the one containing the
                  -- @project.cabal@ file
                  -> FilePath
                  -- ^ Path to the @dist/@ directory
                  -> m ()
writeAutogenFiles readProc projdir distdir = liftIO $ do
  exe  <- findLibexecExe "cabal-helper-wrapper"
  void $ readProc exe [projdir, distdir, "write-autogen-files"] ""

-- | Get the path to the sandbox package-db in a project
getSandboxPkgDb :: (FilePath -> [String] -> String -> IO String)
             -> FilePath
             -- ^ Cabal build platform, i.e. @buildPlatform@
             -> Version
             -- ^ GHC version (@cProjectVersion@ is your friend)
             -> IO (Maybe FilePath)
getSandboxPkgDb readProc =
    CabalHelper.Sandbox.getSandboxPkgDb $ unsafePerformIO $ buildPlatform readProc

buildPlatform :: (FilePath -> [String] -> String -> IO String) -> IO String
buildPlatform readProc = do
  exe  <- findLibexecExe "cabal-helper-wrapper"
  CabalHelper.Sandbox.dropWhileEnd isSpace <$> readProc exe ["print-build-platform"] ""

-- | This exception is thrown by all 'runQuery' functions if the internal
-- wrapper executable cannot be found. You may catch this and present the user
-- an appropriate error message however the default is to print
-- 'libexecNotFoundError'.
data LibexecNotFoundError = LibexecNotFoundError String FilePath
                          deriving (Typeable)
instance Exception LibexecNotFoundError
instance Show LibexecNotFoundError where
  show (LibexecNotFoundError exe dir) =
    libexecNotFoundError exe dir "https://github.com/DanielG/cabal-helper/issues"

findLibexecExe :: String -> IO FilePath
findLibexecExe "cabal-helper-wrapper" = do
    libexecdir <- getLibexecDir
    let exeName = "cabal-helper-wrapper"
        exe = libexecdir </> exeName <.> exeExtension

    exists <- doesFileExist exe

    if exists
       then return exe
       else do
         mdir <- tryFindCabalHelperTreeLibexecDir
         case mdir of
           Nothing ->
               error $ throw $ LibexecNotFoundError exeName libexecdir
           Just dir ->
               return $ dir </> "dist" </> "build" </> exeName </> exeName
findLibexecExe exe = error $ "findLibexecExe: Unknown executable: " ++ exe

tryFindCabalHelperTreeLibexecDir :: IO (Maybe FilePath)
tryFindCabalHelperTreeLibexecDir = do
  exe <- getExecutablePath'
  dir <- case takeFileName exe of
    "ghc" -> do -- we're probably in ghci; try CWD
        getCurrentDirectory
    _ ->
        return $ (!!4) $ iterate takeDirectory exe
  exists <- doesFileExist $ dir </> "cabal-helper.cabal"
  return $ if exists
             then Just dir
             else Nothing

libexecNotFoundError :: String   -- ^ Name of the executable we were trying to
                                 -- find
                     -> FilePath -- ^ Path to @$libexecdir@
                     -> String   -- ^ URL the user will be directed towards to
                                 -- report a bug.
                     -> String
libexecNotFoundError exe dir reportBug = printf
 ( "Could not find $libexecdir/%s\n"
 ++"\n"
 ++"If you are a developer set the environment variable\n"
 ++"`cabal_helper_libexecdir' to override $libexecdir[1]. The following will\n"
 ++"work in the cabal-helper source tree:\n"
 ++"\n"
 ++"    $ export cabal_helper_libexecdir=$PWD/dist/build/%s\n"
 ++"\n"
 ++"[1]: %s\n"
 ++"\n"
 ++"If you don't know what I'm talking about something went wrong with your\n"
 ++"installation. Please report this problem here:\n"
 ++"\n"
 ++"    %s") exe exe dir reportBug

getExecutablePath' :: IO FilePath
getExecutablePath' =
#if MIN_VERSION_base(4,6,0)
    getExecutablePath
#else
    getProgName
#endif

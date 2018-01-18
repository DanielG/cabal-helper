-- ghc-mod: Making Haskell development *more* fun
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

{-# LANGUAGE CPP, RecordWildCards, FlexibleContexts, ConstraintKinds,
  GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric, DeriveFunctor
 #-}

{-|
Module      : Distribution.Helper
License     : AGPL-3
Maintainer  : dxld@darkboxed.org
Portability : POSIX
-}

module Distribution.Helper (
  -- * Running Queries
    Query
  , runQuery

  -- * Queries against Cabal\'s on disk state

  -- ** Package queries
  , packageId
  , packageDbStack
  , packageFlags
  , compilerVersion

  , ghcMergedPkgOptions

  -- ** cabal-install queries
  , configFlags
  , nonDefaultConfigFlags


  -- ** Component queries
  , ComponentQuery
  , components

  , ghcSrcOptions
  , ghcPkgOptions
  , ghcLangOptions
  , ghcOptions
  , sourceDirs
  , entrypoints
  , needsBuildOutput

  -- * Query environment
  , QueryEnv
  , mkQueryEnv
  , qeReadProcess
  , qePrograms
  , qeProjectDir
  , qeDistDir
  , qeCabalPkgDb
  , qeCabalVer

  , Programs(..)
  , defaultPrograms


  -- * Result types
  , ChModuleName(..)
  , ChComponentName(..)
  , ChPkgDb(..)
  , ChEntrypoint(..)
  , NeedsBuildOutput(..)

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

  -- * Reexports
  , module Data.Functor.Apply
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception as E
import Data.Char
import Data.List
import Data.Maybe
import Data.Version
import Data.Typeable
import Data.Functor.Apply
import Distribution.Simple.BuildPaths (exeExtension)
import System.Environment
import System.FilePath hiding ((<.>))
import qualified System.FilePath as FP
import System.Directory
import System.Process
import System.IO.Unsafe
import Text.Printf
import GHC.Generics
import Prelude

import Paths_cabal_helper (getLibexecDir)
import CabalHelper.Shared.InterfaceTypes
import CabalHelper.Shared.Sandbox

-- | Paths or names of various programs we need.
data Programs = Programs {
      -- | The path to the @cabal@ program.
      cabalProgram  :: FilePath,

      -- | The path to the @ghc@ program.
      ghcProgram    :: FilePath,

      -- | The path to the @ghc-pkg@ program. If
      -- not changed it will be derived from the path to 'ghcProgram'.
      ghcPkgProgram :: FilePath
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

-- | Default all programs to their unqualified names, i.e. they will be searched
-- for on @PATH@.
defaultPrograms :: Programs
defaultPrograms = Programs "cabal" "ghc" "ghc-pkg"

-- | Environment for running a 'Query'. The real constructor is not exposed,
-- the field accessors are however. See below. Use the 'mkQueryEnv' smart
-- constructor to construct one.
data QueryEnv = QueryEnv {
      -- | Field accessor for 'QueryEnv'. Defines how to start the cabal-helper
      --  process. Useful if you need to capture stderr output from the helper.
      qeReadProcess :: FilePath -> [String] -> String -> IO String,

      -- | Field accessor for 'QueryEnv'.
      qePrograms    :: Programs,

      -- | Field accessor for 'QueryEnv'. Defines path to the project directory,
      -- i.e. a directory containing a @project.cabal@ file
      qeProjectDir  :: FilePath,


      -- | Field accessor for 'QueryEnv'. Defines path to the @dist/@ directory,
      -- /builddir/ in Cabal terminology.
      qeDistDir     :: FilePath,

      -- | Field accessor for 'QueryEnv'. Defines where to look for the Cabal
      -- library when linking the helper.
      qeCabalPkgDb  :: Maybe FilePath,

      -- | Field accessor for 'QueryEnv'. If @dist/setup-config@ wasn\'t written
      -- by this version of Cabal an error is thrown when running the query.
      qeCabalVer    :: Maybe Version
    }

-- | @mkQueryEnv projdir distdir@. Smart constructor for 'QueryEnv'.
-- Sets fields 'qeProjectDir' and 'qeDistDir' to @projdir@ and @distdir@
-- respectively and provides sensible defaults for the other fields.
mkQueryEnv :: FilePath
           -- ^ Path to the project directory, i.e. the directory containing a
           -- @project.cabal@ file
           -> FilePath
           -- ^ Path to the @dist/@ directory, called /builddir/ in Cabal
           -- terminology.
           -> QueryEnv
mkQueryEnv projdir distdir = QueryEnv {
    qeReadProcess = readProcess
  , qePrograms    = defaultPrograms
  , qeProjectDir  = projdir
  , qeDistDir     = distdir
  , qeCabalPkgDb  = Nothing
  , qeCabalVer    = Nothing
  }

data SomeLocalBuildInfo = SomeLocalBuildInfo {
      slbiPackageDbStack      :: [ChPkgDb],
      slbiPackageFlags        :: [(String, Bool)],
      slbiCompilerVersion     :: (String, Version),

      slbiGhcMergedPkgOptions :: [String],

      slbiConfigFlags         :: [(String, Bool)],
      slbiNonDefaultConfigFlags :: [(String, Bool)],

      slbiGhcSrcOptions       :: [(ChComponentName, [String])],
      slbiGhcPkgOptions       :: [(ChComponentName, [String])],
      slbiGhcLangOptions      :: [(ChComponentName, [String])],
      slbiGhcOptions          :: [(ChComponentName, [String])],

      slbiSourceDirs          :: [(ChComponentName, [String])],
      slbiEntrypoints         :: [(ChComponentName, ChEntrypoint)],
      slbiNeedsBuildOutput    :: [(ChComponentName, NeedsBuildOutput)]
    } deriving (Eq, Ord, Read, Show)

-- | A lazy, cached, query against a package's Cabal configuration. Use
-- 'runQuery' to execute it.
newtype Query m a = Query { unQuery :: StateT (Maybe SomeLocalBuildInfo)
                                         (ReaderT QueryEnv m) a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans Query where
    lift = Query . lift . lift

type MonadQuery m = ( MonadIO m
                    , MonadState (Maybe SomeLocalBuildInfo) m
                    , MonadReader QueryEnv m)

-- | A 'Query' to run on all components of a package. Use 'components' to get a
-- regular 'Query'.
newtype ComponentQuery m a = ComponentQuery (Query m [(ChComponentName, a)])
    deriving (Functor)

instance (Functor m, Monad m) => Apply (ComponentQuery m) where
    ComponentQuery flab <.> ComponentQuery fla =
        ComponentQuery $ liftM2 go flab fla
      where
        go :: [(ChComponentName, a -> b)]
           -> [(ChComponentName, a)]
           -> [(ChComponentName, b)]
        go lab la =
            [ (cn, ab a)
            | (cn,  ab) <- lab
            , (cn', a)  <- la
            , cn == cn'
            ]

run :: Monad m => QueryEnv -> Maybe SomeLocalBuildInfo -> Query m a -> m a
run e s action = flip runReaderT e (flip evalStateT s (unQuery action))

-- | @runQuery env query@. Run a 'Query' under a given 'QueryEnv'.
runQuery :: Monad m
         => QueryEnv
         -> Query m a
         -> m a
runQuery qe action = run qe Nothing action

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

-- | Like @ghcPkgOptions@ but for the whole package not just one component
ghcMergedPkgOptions :: MonadIO m => Query m [String]

-- | Flag definitions from cabal file
packageFlags :: MonadIO m => Query m [(String, Bool)]

-- | Flag assignments from setup-config
configFlags :: MonadIO m => Query m [(String, Bool)]

-- | Flag assignments from setup-config which differ from the default
-- setting. This can also include flags which cabal decided to modify,
-- i.e. don't rely on these being the flags set by the user directly.
nonDefaultConfigFlags :: MonadIO m => Query m [(String, Bool)]

-- | The version of GHC the project is configured to use
compilerVersion :: MonadIO m => Query m (String, Version)

-- | Package identifier, i.e. package name and version
packageId :: MonadIO m => Query m (String, Version)

-- | Run a ComponentQuery on all components of the package.
components :: Monad m => ComponentQuery m (ChComponentName -> b) -> Query m [b]
components (ComponentQuery sc) = map (\(cn, f) -> f cn) `liftM` sc

-- | Modules or files Cabal would have the compiler build directly. Can be used
-- to compute the home module closure for a component.
entrypoints   :: MonadIO m => ComponentQuery m ChEntrypoint

-- | The component has a non-default module renaming, so needs build output ().
needsBuildOutput :: MonadIO m => ComponentQuery m NeedsBuildOutput

-- | A component's @source-dirs@ field, beware since if this is empty implicit
-- behaviour in GHC kicks in.
sourceDirs    :: MonadIO m => ComponentQuery m [FilePath]

-- | All options Cabal would pass to GHC.
ghcOptions    :: MonadIO m => ComponentQuery m [String]

-- | Only search path related GHC options.
ghcSrcOptions :: MonadIO m => ComponentQuery m [String]

-- | Only package related GHC options, sufficient for things don't need to
-- access any home modules.
ghcPkgOptions :: MonadIO m => ComponentQuery m [String]

-- | Only language related options, i.e. @-XSomeExtension@
ghcLangOptions :: MonadIO m => ComponentQuery m [String]

packageId             = Query $ getPackageId
packageDbStack        = Query $ slbiPackageDbStack        `liftM` getSlbi
packageFlags          = Query $ slbiPackageFlags          `liftM` getSlbi
compilerVersion       = Query $ slbiCompilerVersion       `liftM` getSlbi
ghcMergedPkgOptions   = Query $ slbiGhcMergedPkgOptions   `liftM` getSlbi
configFlags           = Query $ slbiConfigFlags           `liftM` getSlbi
nonDefaultConfigFlags = Query $ slbiNonDefaultConfigFlags `liftM` getSlbi

ghcSrcOptions    = ComponentQuery $ Query $ slbiGhcSrcOptions    `liftM` getSlbi
ghcPkgOptions    = ComponentQuery $ Query $ slbiGhcPkgOptions    `liftM` getSlbi
ghcOptions       = ComponentQuery $ Query $ slbiGhcOptions       `liftM` getSlbi
ghcLangOptions   = ComponentQuery $ Query $ slbiGhcLangOptions   `liftM` getSlbi
sourceDirs       = ComponentQuery $ Query $ slbiSourceDirs       `liftM` getSlbi
entrypoints      = ComponentQuery $ Query $ slbiEntrypoints      `liftM` getSlbi
needsBuildOutput = ComponentQuery $ Query $ slbiNeedsBuildOutput `liftM` getSlbi

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
            ++ if ghcPkgProgram progs /= "ghc-pkg"
                 then [ "--with-ghc-pkg=" ++ ghcPkgProgram progs ]
                 else []
            ++ cabalOpts
    _ <- liftIO $ readProc (cabalProgram progs) ("configure":progOpts) ""
    return ()

readHelper :: (MonadIO m, MonadQuery m) => [String] -> m [Maybe ChResponse]
readHelper args = ask >>= \qe -> liftIO $ do
  out <- either error id <$> invokeHelper qe args
  let res = read out
  liftIO $ evaluate res `E.catch` \se@(SomeException _) -> do
      md <- lookupEnv' "CABAL_HELPER_DEBUG"
      let msg = "readHelper: exception: '" ++ show se ++ "'"
      error $ msg ++ case md of
        Nothing -> ", for more information set the environment variable CABAL_HELPER_DEBUG"
        Just _ -> ", output: '"++ out ++"'"

invokeHelper :: QueryEnv -> [String] -> IO (Either String String)
invokeHelper QueryEnv {..} args = do
  let progArgs = [ "--with-ghc="     ++ ghcProgram qePrograms
                 , "--with-ghc-pkg=" ++ ghcPkgProgram qePrograms
                 , "--with-cabal="   ++ cabalProgram qePrograms
                 ]
  exe  <- findLibexecExe
  let args' = progArgs ++ qeProjectDir:qeDistDir:args
  out <- qeReadProcess exe args' ""
  (Right <$> evaluate out) `E.catch` \(SomeException _) ->
      return $ Left $ concat
                 ["invokeHelper", ": ", exe, " "
                 , intercalate " " (map show args')
                 , " failed"
                 ]

getPackageId :: MonadQuery m => m (String, Version)
getPackageId = ask >>= \QueryEnv {..} -> do
  [ Just (ChResponseVersion pkgName pkgVer) ] <- readHelper [ "package-id" ]
  return (pkgName, pkgVer)

getSomeConfigState :: MonadQuery m => m SomeLocalBuildInfo
getSomeConfigState = ask >>= \QueryEnv {..} -> do
  res <- readHelper
         [ "package-db-stack"
         , "flags"
         , "compiler-version"

         , "ghc-merged-pkg-options"

         , "config-flags"
         , "non-default-config-flags"

         , "ghc-src-options"
         , "ghc-pkg-options"
         , "ghc-lang-options"
         , "ghc-options"

         , "source-dirs"
         , "entrypoints"
         , "needs-build-output"
         ]
  let [ Just (ChResponsePkgDbs      slbiPackageDbStack),
        Just (ChResponseFlags       slbiPackageFlags),
        Just (ChResponseVersion     comp compVer),

        Just (ChResponseList        slbiGhcMergedPkgOptions),

        Just (ChResponseFlags       slbiConfigFlags),
        Just (ChResponseFlags       slbiNonDefaultConfigFlags),

        Just (ChResponseCompList    slbiGhcSrcOptions),
        Just (ChResponseCompList    slbiGhcPkgOptions),
        Just (ChResponseCompList    slbiGhcLangOptions),
        Just (ChResponseCompList    slbiGhcOptions),

        Just (ChResponseCompList    slbiSourceDirs),
        Just (ChResponseEntrypoints slbiEntrypoints),
        Just (ChResponseNeedsBuild  slbiNeedsBuildOutput)
        ] = res
      slbiCompilerVersion = (comp, compVer)
  return $ SomeLocalBuildInfo {..}


-- | Make sure the appropriate helper executable for the given project is
-- installed and ready to run queries.
prepare :: MonadIO m => QueryEnv -> m ()
prepare qe =
  liftIO $ void $ invokeHelper qe []

-- | Create @cabal_macros.h@ and @Paths_\<pkg\>@ possibly other generated files
-- in the usual place.
writeAutogenFiles :: MonadIO m => QueryEnv -> m ()
writeAutogenFiles qe  =
  liftIO $ void $ invokeHelper qe ["write-autogen-files"]

-- | Get the path to the sandbox package-db in a project
getSandboxPkgDb :: (FilePath -> [String] -> String -> IO String)
             -> FilePath
             -- ^ Cabal build platform, i.e. @buildPlatform@
             -> Version
             -- ^ GHC version (@cProjectVersion@ is your friend)
             -> IO (Maybe FilePath)
getSandboxPkgDb readProc =
    CabalHelper.Shared.Sandbox.getSandboxPkgDb $ unsafePerformIO $ buildPlatform readProc

buildPlatform :: (FilePath -> [String] -> String -> IO String) -> IO String
buildPlatform readProc = do
  exe  <- findLibexecExe
  CabalHelper.Shared.Sandbox.dropWhileEnd isSpace <$> readProc exe ["print-build-platform"] ""

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

findLibexecExe :: IO FilePath
findLibexecExe = do
    libexecdir <- getLibexecDir
    let exeName = "cabal-helper-wrapper"
        exe = libexecdir </> exeName FP.<.> exeExtension'

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
 ++"If you are a cabal-helper developer you can set the environment variable\n"
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

lookupEnv' :: String -> IO (Maybe String)
lookupEnv' k = lookup k <$> getEnvironment

exeExtension' :: FilePath
exeExtension' = Distribution.Simple.BuildPaths.exeExtension

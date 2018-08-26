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

{-# LANGUAGE RecordWildCards, FlexibleContexts, ConstraintKinds,
  GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric, DeriveFunctor,
  NamedFieldPuns, OverloadedStrings, ViewPatterns #-}

{-|
Module      : Distribution.Helper
License     : GPL-3
Maintainer  : cabal-helper@dxld.at
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
  , Distribution.Helper.buildPlatform

  -- * Stuff that cabal-install really should export
  , Distribution.Helper.getSandboxPkgDb

  -- * Managing @dist/@
  , prepare
  , reconfigure
  , writeAutogenFiles

  -- * Reexports
  , module Data.Functor.Apply
  ) where

import Cabal.Plan
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Exception as E
import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Version
import qualified Data.Text as Text
import Data.Function
import Data.Functor.Apply
import System.Environment
import System.FilePath hiding ((<.>))
import System.Directory
import System.Process
import Text.Printf
import Text.Show.Pretty
import Prelude


import CabalHelper.Compiletime.Compile
import CabalHelper.Compiletime.Types
import CabalHelper.Shared.InterfaceTypes
import CabalHelper.Shared.Sandbox


import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent, deafening)
import Distribution.Package (packageName, packageVersion)
import Distribution.Simple.GHC as GHC (configure)

import qualified CabalHelper.Compiletime.Compat.ProgramDb as ProgDb
    ( defaultProgramDb, programPath, lookupProgram, ghcProgram, ghcPkgProgram)
import CabalHelper.Compiletime.Compat.Version
import CabalHelper.Shared.Common



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
  out <- invokeHelper qe args
  let res = read out
  liftIO $ evaluate res `E.catch` \se@(SomeException _) -> do
      md <- lookupEnv' "CABAL_HELPER_DEBUG"
      let msg = "readHelper: exception: '" ++ show se ++ "'"
      panicIO $ msg ++ case md of
        Nothing -> ", for more information set the environment variable CABAL_HELPER_DEBUG"
        Just _ -> ", output: '"++ out ++"'"

invokeHelper :: QueryEnv -> [String] -> IO String
invokeHelper QueryEnv {..} args0 = do
  let opts = defaultCompileOptions
             { oPrograms = qePrograms
             , oCabalPkgDb = PackageDbDir <$> qeCabalPkgDb }

  opts' <- overrideVerbosityEnvVar =<< guessProgramPaths opts

  exe <- wrapperV1 opts' qeProjectDir qeDistDir

  let args1 = qeProjectDir : qeDistDir : args0

  out <- qeReadProcess exe args1 ""
  evaluate out `E.catch` \(SomeException _) ->
      panicIO $ concat
        ["invokeHelper", ": ", exe, " "
        , intercalate " " (map show args1)
        , " failed!"
        ]

getPackageId :: (MonadQuery m, MonadIO m) => m (String, Version)
getPackageId = ask >>= \QueryEnv {..} -> liftIO $ do
  let v = silent
  [cfile] <- filter isCabalFile <$> getDirectoryContents qeProjectDir
  gpd <- readPackageDescription v (qeProjectDir </> cfile)
  return $ (display (packageName gpd), toDataVersion (packageVersion gpd))

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
getSandboxPkgDb
    :: String
    -- ^ Cabal build platform, i.e. @buildPlatform@
    -> Version
    -- ^ GHC version (@cProjectVersion@ is your friend)
    -> FilePath
    -- ^ Path to the project directory, i.e. a directory containing a
    -- @cabal.sandbox.config@ file
    -> IO (Maybe FilePath)
getSandboxPkgDb buildPlat ghc_ver projdir =
    CabalHelper.Shared.Sandbox.getSandboxPkgDb buildPlat ghc_ver projdir

buildPlatform :: String
buildPlatform = display Distribution.System.buildPlatform

lookupEnv' :: String -> IO (Maybe String)
lookupEnv' k = lookup k <$> getEnvironment


guessProgramPaths :: CompileOptions -> IO CompileOptions
guessProgramPaths opts = do
    let v | oVerbose opts = deafening
          | otherwise     = silent

        mGhcPath0    | same ghcProgram progs dprogs = Nothing
                     | otherwise = Just $ ghcProgram progs
        mGhcPkgPath0 | same ghcPkgProgram progs dprogs = Nothing
                     | otherwise = Just $ ghcPkgProgram progs

    (_compiler, _mplatform, progdb)
        <- GHC.configure
               v
               mGhcPath0
               mGhcPkgPath0
               ProgDb.defaultProgramDb
    let getProg p = ProgDb.programPath <$> ProgDb.lookupProgram p progdb
        mghcPath1    = getProg ProgDb.ghcProgram
        mghcPkgPath1 = getProg ProgDb.ghcPkgProgram

        progs' = progs
          { ghcProgram    = fromMaybe (ghcProgram progs) mghcPath1
          , ghcPkgProgram = fromMaybe (ghcProgram progs) mghcPkgPath1
          }
    return opts { oPrograms = progs' }
 where
   same f o o'  = f o == f o'
   progs = oPrograms opts
   dprogs = defaultPrograms

overrideVerbosityEnvVar :: CompileOptions -> IO CompileOptions
overrideVerbosityEnvVar opts = do
  x <- lookup  "CABAL_HELPER_DEBUG" <$> getEnvironment
  return $ case x of
    Just _  -> opts { oVerbose = True }
    Nothing -> opts

wrapperV1
    :: CompileOptions
    -> FilePath
    -> FilePath
    -> IO FilePath
wrapperV1 opts projdir distdir = do
  cfgf <- canonicalizePath (distdir </> "setup-config")
  mhdr <- getCabalConfigHeader cfgf
  case (mhdr, oCabalVersion opts) of
    (Nothing, _) -> panicIO $ printf "\
\Could not read Cabal's persistent setup configuration header\n\
\- Check first line of: %s\n\
\- Maybe try: $ cabal configure" cfgf
    (Just (hdrCabalVersion, _), Just ver)
      | hdrCabalVersion /= ver -> panicIO $ printf "\
\Cabal version %s was requested but setup configuration was\n\
\written by version %s" (showVersion ver) (showVersion hdrCabalVersion)
    (Just (hdrCabalVersion, _), _) -> do
      compileHelper' opts hdrCabalVersion projdir Nothing distdir

wrapperV2
    :: CompileOptions
    -> FilePath
    -> FilePath
    -> UnitId
    -> IO (FilePath, FilePath)
wrapperV2 opts projdir distdir unitid@(UnitId (Text.unpack -> unitid')) = do
  let plan_path = distdir </> "cache" </> "plan.json"
  plan@PlanJson {pjCabalLibVersion=Ver (makeDataVersion -> pjCabalLibVersion) }
      <- decodePlanJson plan_path
  case oCabalVersion opts of
    Just ver | pjCabalLibVersion /= ver -> let
        sver = showVersion ver
        spjVer = showVersion pjCabalLibVersion
      in panicIO $ printf "\
\Cabal version %s was requested but plan.json was written by version %s" sver spjVer
    _ -> case Map.lookup unitid $ pjUnits plan of
      Just u@Unit {uType} | uType /= UnitTypeLocal -> do
        panicIO $ "\
\UnitId '"++ unitid' ++"' points to non-local unit: " ++ ppShow u
      Just Unit {uDistDir=Nothing} -> panicIO $ printf "\
\plan.json doesn't contain 'dist-dir' for UnitId '"++ unitid' ++"'"
      Just Unit {uType=UnitTypeLocal, uDistDir=Just distdirv1} -> do
        exe <- compileHelper' opts pjCabalLibVersion projdir (Just (plan, distdir)) distdirv1
        return (exe, distdirv1)
      _ -> let
          units = map (\(UnitId u) -> Text.unpack u)
                $ Map.keys
                $ Map.filter ((==UnitTypeLocal) . uType)
                $ pjUnits plan
          units_list = unlines $ map ("  "++) units
        in
          panicIO $ "\
\UnitId '"++ unitid' ++"' not found in plan.json, available local units:\n" ++ units_list


compileHelper'
    :: CompileOptions
    -> Version
    -> FilePath
    -> Maybe (PlanJson, FilePath)
    -> FilePath
    -> IO FilePath
compileHelper' opts pjCabalLibVersion projdir mnewstyle distdirv1 = do
  eexe <- compileHelper opts pjCabalLibVersion projdir mnewstyle distdirv1
  case eexe of
    Left rv ->
        panicIO $ "compileHelper': compiling helper failed! (exit code "++ show rv
    Right exe ->
        return exe

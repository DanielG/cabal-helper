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
  StandaloneDeriving, NamedFieldPuns, OverloadedStrings, ViewPatterns,
  TupleSections, TypeFamilies, DataKinds, GADTs, ScopedTypeVariables,
  ImplicitParams, RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-|
Module      : Distribution.Helper
License     : GPL-3
Maintainer  : cabal-helper@dxld.at
Portability : POSIX
-}

module Distribution.Helper (
  -- * Type Variable Naming Conventions
  -- $type-conventions

  -- * Running Queries
    Query
  , runQuery

  -- * Queries against Cabal\'s on disk state

  -- ** Project queries
  , compilerVersion
  , projectUnits

  -- ** Unit queries
  , Unit -- abstract
  , uComponentName
  , UnitId -- abstract
  , UnitInfo(..)
  , unitInfo

  -- ** Convenience Queries
  , allUnits

  -- * Query environment
  , QueryEnv -- abstract
  , mkQueryEnv
  , qeReadProcess
  , qePrograms
  , qeCompPrograms
  , qeProjLoc
  , qeDistDir

  -- * GADTs
  , DistDir(..)
  , ProjType(..)
  , SProjType(..)
  , ProjLoc(..)

  , Programs(..)
  , defaultPrograms


  -- * Result types
  , ChComponentInfo(..)
  , ChComponentName(..)
  , ChModuleName(..)
  , ChPkgDb(..)
  , ChEntrypoint(..)
  , NeedsBuildOutput(..)

  -- * General information
  , Distribution.Helper.buildPlatform

  -- * Stuff that cabal-install really should export
  , Distribution.Helper.getSandboxPkgDb

  -- * Managing @dist/@
  , prepare
  , writeAutogenFiles
  ) where

import Cabal.Plan hiding (Unit, UnitId, uDistDir)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Exception as E
import Data.IORef
import Data.List hiding (filter)
import Data.String
import qualified Data.Text as Text
import Data.Maybe
import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Version
import Data.Function
import System.Clock as Clock
import System.Environment
import System.FilePath
import System.Directory
import System.Process
import System.Posix.Types
import System.PosixCompat.Files
import Text.Printf
import Prelude

import CabalHelper.Compiletime.Compile
import qualified CabalHelper.Compiletime.Program.Stack as Stack
import qualified CabalHelper.Compiletime.Program.GHC as GHC
import qualified CabalHelper.Compiletime.Program.CabalInstall as CabalInstall
import CabalHelper.Compiletime.Cabal
import CabalHelper.Compiletime.Log
import CabalHelper.Compiletime.Process
import CabalHelper.Compiletime.Sandbox
import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.InterfaceTypes
import CabalHelper.Shared.Common

import CabalHelper.Compiletime.Compat.Version
import qualified CabalHelper.Compiletime.Compat.ProgramDb as ProgDb
    ( defaultProgramDb, programPath, lookupProgram, ghcProgram, ghcPkgProgram)

import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent, deafening)
import Distribution.Simple.GHC as GHC (configure)

-- $type-conventions
-- Throughout the API we use the following conventions for type variables:
--
-- * @pt@ stands for "project type", when instantiated it is always of kind
--   'ProjType'.
--
-- * @c@ stands for "cache". It is used internally to make the cache
--   inaccessible for some parts of the implementation. Users of the API may
--   completely ignore this parameter. See the internal 'qeCacheRef' field
--   accessor of 'QueryEnv' for details.


-- | A lazy, cached, query against a package's Cabal configuration. Use
-- 'runQuery' to execute it.
newtype Query pt a = Query
    { runQuery :: QueryEnv pt -> IO a
    -- ^ @runQuery env query@. Run a 'Query' under a given 'QueryEnv.
    }

instance Functor (Query pt) where
    fmap = liftM

instance Applicative (Query pt) where
    (<*>) = ap
    pure = return

instance Monad (Query pt) where
    (Query ma) >>= amb = Query $ \qe -> ma qe >>= \a -> runQuery (amb a) qe
    return a = Query $ const $ return a

-- | @mkQueryEnv projdir distdir@. Smart constructor for 'QueryEnv'.
-- Sets fields 'qeProjLoc' and 'qeDistDir' to @projdir@ and @distdir@
-- respectively and provides sensible defaults for the other fields.
mkQueryEnv
    :: ProjLoc pt
    -- ^ Location of the project.
    -> DistDir pt
    -- ^ Path to the @dist/@ or @dist-newstyle/@ directory, called
    -- /builddir/ in Cabal terminology.
    -> IO (QueryEnv pt)
mkQueryEnv projloc distdir = do
  cr <- newIORef $ QueryCache Nothing Map.empty
  return $ QueryEnv
    { qeReadProcess = \stdin mcwd exe args ->
        readCreateProcess (proc exe args){ cwd = mcwd } stdin
    , qeCallProcess  = \mcwd exe args -> do
        let ?verbose = False -- TODO: we should get this from env or something
        callProcessStderr mcwd exe args
    , qePrograms     = defaultPrograms
    , qeCompPrograms = defaultCompPrograms
    , qeProjLoc      = projloc
    , qeDistDir      = distdir
    , qeCacheRef     = cr
    }

-- | Construct paths to project configuration files.
projConf :: ProjLoc pt -> ProjConf pt
projConf (ProjLocCabalFile cabal_file) =
   ProjConfV1 cabal_file
projConf (ProjLocV2Dir projdir_path) =
  projConf $ ProjLocV2File $ projdir_path </> "cabal.project"
projConf (ProjLocV2File proj_file) =
  ProjConfV2
    { pcV2CabalProjFile       = proj_file
    , pcV2CabalProjLocalFile  = proj_file <.> "local"
    , pcV2CabalProjFreezeFile = proj_file <.> "freeze"
    }
projConf (ProjLocStackYaml stack_yaml) =
  ProjConfStack
    { pcStackYaml = stack_yaml }

getProjConfModTime :: ProjConf pt -> IO ProjConfModTimes
getProjConfModTime ProjConfV1{pcV1CabalFile} =
  fmap ProjConfModTimes $ mapM getFileModTime
    [ pcV1CabalFile
    ]
getProjConfModTime ProjConfV2{..} = do
  fmap (ProjConfModTimes . catMaybes) $
    mapM (traverse getFileModTime <=< mightExist)
      [ pcV2CabalProjFile
      , pcV2CabalProjLocalFile
      , pcV2CabalProjFreezeFile
      ]
getProjConfModTime ProjConfStack{..} =
  fmap ProjConfModTimes $ mapM getFileModTime
    [ pcStackYaml
    ]

getUnitModTimes :: Unit pt -> IO UnitModTimes
getUnitModTimes
  Unit
    { uDistDir=DistDirLib distdirv1
    , uCabalFile=CabalFile cabal_file_path
    , uPackageDir
    , uImpl
    }
  = do
    umtPkgYaml <-
        case uImpl of
          UnitImplStack{}
            -> traverse getFileModTime =<< mightExist package_yaml_path
          _ -> return Nothing
    umtCabalFile <- getFileModTime cabal_file_path
    umtSetupConfig <- (traverse getFileModTime <=< mightExist) setup_config_path
    return UnitModTimes {..}
  where
    package_yaml_path = uPackageDir  </> "package.yaml"
    setup_config_path = distdirv1 </> "setup-config"


-- | The version of GHC the project is configured to use for compilation.
compilerVersion       :: Query pt (String, Version)
compilerVersion = Query $ \qe ->
  getProjInfo qe >>= \proj_info ->
    let someUnit = NonEmpty.head $ piUnits proj_info in
    --  ^ TODO: ASSUMPTION: Here we assume the compiler version is uniform
    --  across all units so here we just pick any one. I'm not sure this is true
    --  for Stack.
    case piImpl proj_info of
      ProjInfoV1 -> uiCompilerId <$> getUnitInfo qe someUnit
      ProjInfoV2 { piV2CompilerId } -> return piV2CompilerId
      ProjInfoStack {} -> uiCompilerId <$> getUnitInfo qe someUnit

-- | All units currently active in a project\'s build plan.
projectUnits          :: Query pt (NonEmpty (Unit pt))
projectUnits = Query $ \qe -> piUnits <$> getProjInfo qe

-- | Get the 'UnitInfo' for a given 'Unit'. To get a 'Unit' see 'projectUnits'.
unitInfo              :: Unit pt -> Query pt UnitInfo
unitInfo u = Query $ \qe -> getUnitInfo qe u

-- | Get information on all units in a project.
allUnits :: (UnitInfo -> a) -> Query pt (NonEmpty a)
allUnits f = fmap f <$> (mapM unitInfo =<< projectUnits)

getProjInfo :: QueryEnv pt -> IO (ProjInfo pt)
getProjInfo qe@QueryEnv{..} = do
  cache@QueryCache{qcProjInfo, qcUnitInfos} <- readIORef qeCacheRef
  proj_info <- checkUpdateProjInfo qe qcProjInfo
  let active_units = NonEmpty.toList $ piUnits proj_info
  writeIORef qeCacheRef $ cache
    { qcProjInfo  = Just proj_info
    , qcUnitInfos = discardInactiveUnitInfos active_units qcUnitInfos
    }
  return proj_info

checkUpdateProjInfo
    :: QueryEnvI c pt
    -> Maybe (ProjInfo pt)
    -> IO (ProjInfo pt)
checkUpdateProjInfo qe mproj_info = do
  let proj_conf = projConf (qeProjLoc qe)
  mtime <- getProjConfModTime proj_conf
  case mproj_info of
    Nothing -> reconf proj_conf mtime
    Just proj_info
        | piProjConfModTimes proj_info /= mtime
            -> reconf proj_conf mtime
        | otherwise
            -> return proj_info
  where
    reconf proj_conf mtime = do
      shallowReconfigureProject qe
      readProjInfo qe proj_conf mtime

getUnitInfo :: QueryEnv pt -> Unit pt -> IO UnitInfo
getUnitInfo qe@QueryEnv{..} unit@Unit{uDistDir} = do
  proj_info <- getProjInfo qe
  cache@QueryCache{qcUnitInfos} <- readIORef qeCacheRef
  let munit_info = Map.lookup uDistDir qcUnitInfos
  unit_info <- checkUpdateUnitInfo qe proj_info unit munit_info
  writeIORef qeCacheRef $ cache
    { qcUnitInfos = Map.insert uDistDir unit_info qcUnitInfos }
  return unit_info

checkUpdateUnitInfo
    :: QueryEnvI c pt
    -> ProjInfo pt
    -> Unit pt
    -> Maybe UnitInfo
    -> IO UnitInfo
checkUpdateUnitInfo qe proj_info unit munit_info = do
  unit_mtimes <- getUnitModTimes unit
  case munit_info of
    Nothing -> reconf
    Just unit_info
      | uiModTimes unit_info /= unit_mtimes
        -> reconf
      | otherwise
        -> return unit_info
  where
    reconf = do
      reconfigureUnit qe unit
      helper <- getHelperExe proj_info qe
      readUnitInfo qe helper unit

-- | Restrict 'UnitInfo' cache to units that are still active
discardInactiveUnitInfos
    :: [Unit pt]
    -> Map DistDirLib UnitInfo
    -> Map DistDirLib UnitInfo
discardInactiveUnitInfos active_units uis0 =
    restrictKeysMap uis0 $ Set.fromList $ map uDistDir active_units
  where
    restrictKeysMap :: Ord k => Map k a -> Set k -> Map k a
    restrictKeysMap m s = Map.filterWithKey (\k _ -> Set.member k s) m


-- | Regenerate project level information by calling the appropriate build
-- system (@cabal@ or @stack@).
shallowReconfigureProject :: QueryEnvI c pt -> IO ()
shallowReconfigureProject QueryEnv
  { qeProjLoc = ProjLocCabalFile _cabal_file
  , qeDistDir = DistDirV1 _distdirv1 } =
    return ()
shallowReconfigureProject QueryEnv
  { qeProjLoc = ProjLocV2File projfile
  , qeDistDir = DistDirV2 _distdirv2, .. } = do
    let projdir = takeDirectory projfile
    _ <- qeCallProcess (Just projdir) (cabalProgram qePrograms)
           ["new-build", "--dry-run", "--project-file="++projfile, "all"]
    return ()
shallowReconfigureProject QueryEnv
  { qeProjLoc = ProjLocV2Dir projdir
  , qeDistDir = DistDirV2 _distdirv2, .. } = do
    _ <- qeCallProcess (Just projdir) (cabalProgram qePrograms)
           ["new-build", "--dry-run", "all"]
    return ()
shallowReconfigureProject QueryEnv
  { qeProjLoc = ProjLocStackYaml _stack_yaml, .. } = do
    -- -- In case we ever need to read the cabal files before the Unit stage, this command regenerates them from package.yaml
    -- _ <- liftIO $ qeCallProcess (Just projdir) (stackProgram qePrograms)
    --        ["build", "--dry-run"] ""
    return ()

reconfigureUnit :: QueryEnvI c pt -> Unit pt -> IO ()
reconfigureUnit QueryEnv{qeDistDir=DistDirV1{}, ..} Unit{uPackageDir=_} = do
  return ()
reconfigureUnit
  QueryEnv{qeProjLoc=ProjLocV2File projfile, ..}
  Unit{uPackageDir, uImpl}
  = do
  _ <- qeCallProcess (Just uPackageDir) (cabalProgram qePrograms)
        (["new-build", "--project-file="++projfile]
         ++ uiV2Components uImpl)
  return ()
reconfigureUnit
  QueryEnv{qeProjLoc=ProjLocV2Dir{}, ..}
  Unit{uPackageDir, uImpl}
  = do
  _ <- qeCallProcess (Just uPackageDir) (cabalProgram qePrograms)
        (["new-build"] ++ uiV2Components uImpl)
        -- TODO: version check for --only-configure
  return ()
reconfigureUnit
  qe@QueryEnv{qeProjLoc=ProjLocStackYaml stack_yaml, ..}
  Unit{uPackageDir}
  = do
  _ <- Stack.callStackCmd qe (Just uPackageDir)
         ["--stack-yaml="++stack_yaml, "build", "--only-configure", "."]
  return ()

getFileModTime :: FilePath -> IO (FilePath, EpochTime)
getFileModTime f = do
  t <- modificationTime <$> getFileStatus f
  return (f, t)

readProjInfo
    :: QueryEnvI c pt -> ProjConf pt -> ProjConfModTimes -> IO (ProjInfo pt)
readProjInfo qe pc pcm = withVerbosity $ do
  let projloc = qeProjLoc qe
  case (qeDistDir qe, pc) of
    (DistDirV1 distdir, ProjConfV1{pcV1CabalFile}) -> do
      let projdir = plV1Dir projloc
      setup_config_path <- canonicalizePath (distdir </> "setup-config")
      mhdr <- getCabalConfigHeader setup_config_path
      case mhdr of
        Nothing ->
          panicIO $ printf "Could not read '%s' header" setup_config_path
        Just (hdrCabalVersion, _) ->
          return ProjInfo
            { piCabalVersion = hdrCabalVersion
            , piProjConfModTimes = pcm
            , piUnits = (:|[]) $ Unit
              { uUnitId = UnitId ""
              , uPackageDir = projdir
              , uCabalFile = CabalFile pcV1CabalFile
              , uDistDir = DistDirLib distdir
              , uImpl = UnitImplV1
              }
            , piImpl = ProjInfoV1
            }
    (DistDirV2 distdirv2, _) -> do
      let plan_path = distdirv2 </> "cache" </> "plan.json"
      plan_mtime <- modificationTime <$> getFileStatus plan_path
      plan@PlanJson { pjCabalLibVersion=Ver pjCabalLibVersion
                    , pjCompilerId=PkgId (PkgName compName) (Ver compVer)
                    }
          <- decodePlanJson plan_path
      Just units <- NonEmpty.nonEmpty <$> CabalInstall.planUnits plan
      return ProjInfo
        { piCabalVersion = makeDataVersion pjCabalLibVersion
        , piProjConfModTimes = pcm
        , piUnits = units
        , piImpl = ProjInfoV2
          { piV2Plan = plan
          , piV2PlanModTime = plan_mtime
          , piV2CompilerId = (Text.unpack compName, makeDataVersion compVer)
          }
        }
    (DistDirStack{}, _) -> do
      Just cabal_files <- NonEmpty.nonEmpty <$> Stack.listPackageCabalFiles qe
      units <- mapM (Stack.getUnit qe) cabal_files
      proj_paths <- Stack.projPaths qe
      let piImpl = ProjInfoStack { piStackProjPaths = proj_paths }
      Just (cabalVer:_) <- withProgs piImpl qe $ runMaybeT $
        GHC.listCabalVersions (Just (sppGlobalPkgDb proj_paths))
        --  ^ See [Note Stack Cabal Version]
      return ProjInfo
        { piCabalVersion = cabalVer
        , piProjConfModTimes = pcm
        , piUnits = units
        , ..
        }

readUnitInfo :: QueryEnvI c pt -> FilePath -> Unit pt -> IO UnitInfo
readUnitInfo
  qe exe unit@Unit {uUnitId=uiUnitId, uCabalFile, uDistDir} = do
    res <- readHelper qe exe uCabalFile uDistDir
           [ "package-id"
           , "package-db-stack"
           , "flags"
           , "compiler-id"
           , "config-flags"
           , "non-default-config-flags"
           , "component-info"
           ]
    let [ Just (ChResponseVersion        uiPackageId),
          Just (ChResponsePkgDbs         uiPackageDbStack),
          Just (ChResponseFlags          uiPackageFlags),
          Just (ChResponseVersion        uiCompilerId),
          Just (ChResponseFlags          uiConfigFlags),
          Just (ChResponseFlags          uiNonDefaultConfigFlags),
          Just (ChResponseComponentsInfo uiComponents)
          ] = res
    uiModTimes <- getUnitModTimes unit
    return $ UnitInfo {..}

readHelper
    :: QueryEnvI c pt
    -> FilePath
    -> CabalFile
    -> DistDirLib
    -> [String]
    -> IO [Maybe ChResponse]
readHelper qe exe cabal_file distdir args = do
  out <- invokeHelper qe exe cabal_file distdir args
  let res :: [Maybe ChResponse]
      res = read out
  liftIO $ evaluate res `E.catch` \ex@ErrorCall{} -> do
      md <- lookupEnv' "CABAL_HELPER_DEBUG"
      let msg = "readHelper: exception: '" ++ show ex ++ "'"
      panicIO $ msg ++ case md of
        Nothing -> "\n  for more information set the environment variable CABAL_HELPER_DEBUG and try again"
        Just _ -> "\n  output:\n'"++ out ++"'"

invokeHelper
    :: QueryEnvI c pt
    -> FilePath
    -> CabalFile
    -> DistDirLib
    -> [String]
    -> IO String
invokeHelper
  QueryEnv {..}
  exe
  (CabalFile cabal_file_path)
  (DistDirLib distdir)
  args0
  = do
    let args1 = cabal_file_path : distdir : args0
    evaluate =<< qeReadProcess "" Nothing exe args1 `E.catch`
      \(_ :: E.IOException) ->
        panicIO $ concat
          ["invokeHelper", ": ", exe, " "
          , intercalate " " (map show args1)
          , " failed!"
          ]

-- | Make sure the appropriate helper executable for the given project is
-- installed and ready to run queries.
prepare :: QueryEnv pt -> IO ()
prepare qe = do
  proj_info <- getProjInfo qe
  void $ getHelperExe proj_info qe

-- | Create @cabal_macros.h@ and @Paths_\<pkg\>@ possibly other generated files
-- in the usual place. See 'Distribution.Simple.Build.initialBuildSteps'.
writeAutogenFiles :: Unit pt -> Query pt ()
writeAutogenFiles Unit{uCabalFile, uDistDir} = Query $ \qe -> do
  proj_info <- getProjInfo qe
  exe <- getHelperExe proj_info qe
  void $ invokeHelper qe exe uCabalFile uDistDir ["write-autogen-files"]

-- | Get the path to the sandbox package-db in a project
getSandboxPkgDb
    :: String
    -- ^ Cabal build platform, i.e. @buildPlatform@
    -> GHC.GhcVersion
    -- ^ GHC version (@cProjectVersion@ is your friend)
    -> FilePath
    -- ^ Path to the project directory, i.e. a directory containing a
    -- @cabal.sandbox.config@ file
    -> IO (Maybe FilePath)
getSandboxPkgDb buildPlat ghcVer projdir =
  CabalHelper.Compiletime.Sandbox.getSandboxPkgDb buildPlat ghcVer projdir

buildPlatform :: String
buildPlatform = display Distribution.System.buildPlatform

lookupEnv' :: String -> IO (Maybe String)
lookupEnv' k = lookup k <$> getEnvironment

withVerbosity :: (Verbose => IO a) -> IO a
withVerbosity act = do
  x <- lookup  "CABAL_HELPER_DEBUG" <$> getEnvironment
  let ?verbose =
        case x of
          Just xs | not (null xs) -> True
          _ -> False
  act

-- | Bring 'Programs' and 'CompPrograms' into scope as implicit parameters
withProgs
    :: Verbose => ProjInfoImpl pt -> QueryEnvI c pt -> (Env => IO a) -> IO a
withProgs impl QueryEnv{..} f = do
  cprogs <- guessCompProgramPaths $ case impl of
    ProjInfoStack projPaths ->
      Stack.patchCompPrograms projPaths qeCompPrograms
    _ -> qeCompPrograms
  let ?cprogs = cprogs in
    let ?progs = qePrograms in f
  where
    -- | Determine ghc-pkg path from ghc path
    guessCompProgramPaths :: Verbose => CompPrograms -> IO CompPrograms
    guessCompProgramPaths progs = do
        let v | ?verbose  = deafening
              | otherwise = silent
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
        return progs
          { ghcProgram    = fromMaybe (ghcProgram progs) mghcPath1
          , ghcPkgProgram = fromMaybe (ghcProgram progs) mghcPkgPath1
          }
      where
        same f o o'  = f o == f o'
        dprogs = defaultCompPrograms

getHelperExe
    :: ProjInfo pt -> QueryEnvI c pt -> IO FilePath
getHelperExe proj_info qe@QueryEnv{..} = do
  withVerbosity $ withProgs (piImpl proj_info) qe $ do
    let comp = wrapper' qeProjLoc qeDistDir proj_info
    let ?progs = qePrograms
        ?cprogs = qeCompPrograms
    t0 <- Clock.getTime Monotonic
    eexe <- compileHelper comp
    t1 <- Clock.getTime Monotonic
    let dt = (/10e9) $ fromInteger $ Clock.toNanoSecs $ Clock.diffTimeSpec t0 t1
        dt :: Float
    vLog $ printf "compileHelper took %.5fs" dt
    case eexe of
      Left rv ->
        panicIO $ "compileHelper': compiling helper failed! exit code "++ show rv
      Right exe ->
        return exe

wrapper'
    :: Verbose
    => ProjLoc pt
    -> DistDir pt
    -> ProjInfo pt
    -> CompHelperEnv
wrapper'
  projloc
  (DistDirV1 distdir)
  ProjInfo{piCabalVersion}
  = CompHelperEnv
    { cheCabalVer = CabalVersion piCabalVersion
    , cheProjDir  = plV1Dir projloc
    , cheProjLocalCacheDir = distdir
    , chePkgDb    = Nothing
    , chePlanJson = Nothing
    , cheDistV2 = Nothing
    }
wrapper'
  projloc
  (DistDirV2 distdir)
  ProjInfo{piImpl=ProjInfoV2{piV2Plan=plan}}
  = case projloc of
      ProjLocV2Dir projdir ->
        let cheProjDir  = projdir in
        CompHelperEnv {..}
      ProjLocV2File proj_file ->
        let cheProjDir = takeDirectory proj_file in
        CompHelperEnv {..}
  where
    cheCabalVer = CabalVersion $ makeDataVersion pjCabalLibVersion
    cheProjLocalCacheDir = distdir </> "cache"
    chePkgDb    = Nothing
    chePlanJson = Just plan
    cheDistV2   = Just distdir
    PlanJson {pjCabalLibVersion=Ver pjCabalLibVersion } = plan
wrapper'
  (ProjLocStackYaml stack_yaml)
  (DistDirStack mworkdir)
  ProjInfo
    { piCabalVersion
    , piImpl = ProjInfoStack
      { piStackProjPaths=StackProjPaths
        { sppGlobalPkgDb }
      }
    }
  = let workdir = fromMaybe ".stack-work" $ unRelativePath <$> mworkdir in
    let projdir = takeDirectory stack_yaml in
    CompHelperEnv
    { cheCabalVer = CabalVersion $ piCabalVersion
    , cheProjDir  = projdir
    , cheProjLocalCacheDir = projdir </> workdir
    , chePkgDb    = Just sppGlobalPkgDb
    , chePlanJson = Nothing
    , cheDistV2 = Nothing
    }

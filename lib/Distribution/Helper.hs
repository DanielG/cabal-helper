-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2019  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE RecordWildCards, FlexibleContexts, ConstraintKinds,
  GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric, DeriveFunctor,
  StandaloneDeriving, NamedFieldPuns, OverloadedStrings, ViewPatterns,
  TupleSections, TypeFamilies, DataKinds, GADTs, ScopedTypeVariables,
  ImplicitParams, RankNTypes, MultiWayIf #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-|
Module      : Distribution.Helper
License     : Apache-2.0
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
  , projectPackages

  -- ** 'Package' queries
  , Package -- abstract
  , pPackageName
  , pSourceDir
  , pUnits

  -- ** 'Unit' queries
  , Unit -- abstract
  , uComponentName
  , UnitId -- abstract
  , UnitInfo(..)
  , unitInfo

  -- ** Convenience Queries
  , allUnits

  -- * Query environment
  , QueryEnv
  , QueryEnvI -- abstract
  , mkQueryEnv
  , qeReadProcess
  , qeCallProcess
  , qePrograms
  , qeProjLoc
  , qeDistDir

  -- * GADTs
  , ProjType(..)
  , CabalProjType(..)
  , ProjLoc(..)
  , DistDir(..)
  , SProjType(..)
  , demoteSProjType
  , projTypeOfDistDir
  , projTypeOfProjLoc
  , SCabalProjType(..)
  , Ex(..)

  -- * Programs
  , Programs(..)
  , defaultPrograms
  , EnvOverride(..)

  -- * Query result types
  , ChComponentInfo(..)
  , ChComponentName(..)
  , ChLibraryName(..)
  , ChModuleName(..)
  , ChPkgDb(..)
  , ChEntrypoint(..)

  -- * General information
  , Distribution.Helper.buildPlatform

  -- * Legacy v1-build helpers
  , Distribution.Helper.getSandboxPkgDb

  -- * Build actions
  , prepare
  , writeAutogenFiles
  , buildProject
  , buildUnits
  ) where

import Cabal.Plan hiding (Unit, UnitId, uDistDir)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Exception as E
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU
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
import qualified Data.Traversable as T
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Version
import Data.Function
import System.Clock as Clock
import System.IO
import System.Environment
import System.FilePath
import System.Directory
import System.Process
import System.Posix.Types
import System.PosixCompat.Files
import Text.Printf
import Text.Read
import Prelude

import CabalHelper.Compiletime.Compile
import qualified CabalHelper.Compiletime.Program.Stack as Stack
import qualified CabalHelper.Compiletime.Program.GHC as GHC
import qualified CabalHelper.Compiletime.Program.CabalInstall as CabalInstall
import CabalHelper.Compiletime.Cabal
import CabalHelper.Compiletime.CompPrograms
import CabalHelper.Compiletime.Log
import CabalHelper.Compiletime.Process
import CabalHelper.Compiletime.Sandbox
import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.InterfaceTypes
import CabalHelper.Shared.Common
import CabalHelper.Runtime.HelperMain (helper_main)

import CabalHelper.Compiletime.Compat.Version

import Distribution.System (buildPlatform)
import Distribution.Text (display)

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


-- | A query against a package's Cabal configuration. Use 'runQuery' to
-- execute it.
newtype Query pt a = Query
    { unQuery :: QueryEnv pt -> IO a
    -- ^ @runQuery env query@. Run a 'Query' under a given 'QueryEnv.
    }

instance Functor (Query pt) where
    fmap = liftM

instance Applicative (Query pt) where
    (<*>) = ap
    pure = return

instance Monad (Query pt) where
    (Query ma) >>= amb = Query $ \qe -> ma qe >>= \a -> unQuery (amb a) qe
    return a = Query $ const $ return a

runQuery :: Query pt a -> QueryEnv pt -> IO a
runQuery (Query action) qe = do
  ckr <- newIORef $ CacheKeyCache Nothing
  let qe' = qe { qeCacheKeys = ckr }
  conf_progs <- getConfProgs qe'
  action qe' { qePrograms = conf_progs }

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
  cr <- newIORef $ QueryCache Nothing Nothing Nothing Map.empty
  return $ QueryEnv
    { qeReadProcess = \stdin mcwd env exe args -> do
        withVerbosity $ readProcessStderr mcwd env exe args ""
    , qeCallProcess  = \mcwd env exe args ->
        withVerbosity $ callProcessStderr mcwd env exe args
    , qePrograms     = defaultPrograms
    , qeProjLoc      = projloc
    , qeDistDir      = distdir
    , qeCacheRef     = cr
    , qeCacheKeys    = error "mkQuery: qeCacheKeys is uninitialized!"
    }

-- | Construct paths to project configuration files given where the project is.
projConf :: ProjLoc pt -> IO (ProjConf pt)
projConf (ProjLocV1Dir pkgdir) =
  ProjConfV1 <$> (complainIfNoCabalFile pkgdir =<< findCabalFile pkgdir)
projConf (ProjLocV1CabalFile cabal_file _) = return $
  ProjConfV1 cabal_file
projConf (ProjLocV2Dir projdir_path) =
  projConf $ ProjLocV2File (projdir_path </> "cabal.project") projdir_path
projConf (ProjLocV2File proj_file _) = return $
  ProjConfV2
    { pcV2CabalProjFile       = proj_file
    , pcV2CabalProjLocalFile  = proj_file <.> "local"
    , pcV2CabalProjFreezeFile = proj_file <.> "freeze"
    }
projConf (ProjLocStackYaml stack_yaml) = return $
  ProjConfStack
    { pcStackYaml = stack_yaml }

-- | Get the current modification-time for each file involved in configuring a
-- project. Optional files in 'ProjConf' are handled by not including them in
-- the result list in 'ProjConfModTimes' if they don\'t exist. This causes the
-- lists to be different if the files end up existing later, which is all we
-- need for cache invalidation.
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
    , uPackage=Package
      { pCabalFile=CabalFile cabal_file_path
      , pSourceDir
      }
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
    package_yaml_path = pSourceDir  </> "package.yaml"
    setup_config_path = distdirv1 </> "setup-config"


-- | The version of GHC the project is configured to use for compilation.
compilerVersion :: Query pt (String, Version)
compilerVersion = Query $ \qe ->
  getProjInfo qe >>= \proj_info ->
    let someUnit = NonEmpty.head $ pUnits $
                   NonEmpty.head $ piPackages proj_info in
    --  ^ ASSUMPTION: Here we assume the compiler version is uniform across all
    --  units so we just pick any one.
    case piImpl proj_info of
      ProjInfoV1 {} -> uiCompilerId <$> getUnitInfo qe someUnit
      ProjInfoV2 { piV2CompilerId } -> return piV2CompilerId
      ProjInfoStack {} -> uiCompilerId <$> getUnitInfo qe someUnit

-- | All local packages currently active in a project\'s build plan.
projectPackages :: Query pt (NonEmpty (Package pt))
projectPackages = Query $ \qe -> piPackages <$> getProjInfo qe

-- | Get the 'UnitInfo' for a given 'Unit'. To get a 'Unit' see 'projectUnits'.
unitInfo :: Unit pt -> Query pt UnitInfo
unitInfo u = Query $ \qe -> getUnitInfo qe u

-- | Get information on all units in a project.
allUnits :: (UnitInfo -> a) -> Query pt (NonEmpty a)
allUnits f = do
  fmap f <$> (T.mapM unitInfo =<< join . fmap pUnits <$> projectPackages)


data Cached c ckc k v = Cached
  { cGet      :: !(c -> Maybe (k, v))
  , cSet      :: !(c -> (k, v) -> c)

  , cGetKey   :: !(ckc -> Maybe k)
  , cSetKey   :: !(ckc -> k -> ckc)

  , cCheckKey :: !(IO k)
  , cKeyValid :: !(k -> k -> Bool)
  -- ^ @cKeyValid old new@ should return 'True' if 'old' is still valid
  -- relative to the value of 'new'.

  , cRegen    :: !(k -> IO v)
  }

-- | Simple caching scheme. Invalidation is based on equality of a "cache
-- key" the current value of which can be got with the IO action 'cGetKey'.
--
-- Note that we only check the actual value of the cache key once per
-- 'runQuery' call by saving the cache key in an ephemeral map.
cached :: QueryEnvI (QueryCacheI a b c d) pt
       -> Cached (QueryCacheI a b c d pt) (CacheKeyCache pt) k v
       -> IO v
cached qe Cached{..} = do
  c <- readIORef (qeCacheRef qe)
  (c', v) <- checkUpdate c (cGet c)
  writeIORef (qeCacheRef qe) c'
  return v
 where
  checkUpdate c m = do
    ckc <- readIORef (qeCacheKeys qe)
    let regen ck = (ck,) <$> cRegen ck
    n <- case m of
      Nothing -> do
        ck <- cCheckKey
        writeIORef (qeCacheKeys qe) (cSetKey ckc ck)
        regen ck
      Just old@(old_ck, old_v) -> do
        ck <- case cGetKey ckc of
          Just cck ->
            return cck -- TODO: skip valid check below in this case
          Nothing -> do
            ck <- cCheckKey
            writeIORef (qeCacheKeys qe) (cSetKey ckc ck)
            return ck
        if
          | cKeyValid old_ck ck -> return old
          | otherwise -> regen ck
    return (cSet c n, snd n)

getProjConfAndModTime :: QueryEnvI c pt -> IO (ProjConf pt, ProjConfModTimes)
getProjConfAndModTime qe = do
  proj_conf <- projConf (qeProjLoc qe)
  mtime <- getProjConfModTime proj_conf
  return (proj_conf, mtime)

getPreInfo :: QueryEnvI (QCPreInfo a b c) pt -> IO (PreInfo pt)
getPreInfo qe =
  cached qe $ Cached
    { cGet = qcPreInfo
    , cSet = \a b -> a { qcPreInfo = Just b }
    , cGetKey = ckcProjConf
    , cSetKey = \a b -> a { ckcProjConf = Just b }
    , cCheckKey = getProjConfAndModTime qe
    , cKeyValid = (==) `on` snd
    , cRegen = \_k -> readPreInfo qe
    }

readPreInfo :: QueryEnvI c pt -> IO (PreInfo pt)
readPreInfo qe = do
  case projTypeOfQueryEnv qe of
    SStack -> do
      piStackProjPaths <- Stack.projPaths qe
      return PreInfoStack
        { piStackProjPaths
        }
    (SCabal _) ->
      return PreInfoCabal

getProjInfo :: QueryEnv pt -> IO (ProjInfo pt)
getProjInfo qe = do
  pre_info <- getPreInfo qe
  cached qe $ Cached
    { cGet = qcProjInfo
    , cSet = \c n@(_, proj_info) ->
        let active_units = NonEmpty.toList $ join $
              fmap pUnits $ piPackages proj_info in
        c { qcProjInfo = Just n
          , qcUnitInfos =
               discardInactiveUnitInfos active_units (qcUnitInfos c)
          }
    , cGetKey = ckcProjConf
    , cSetKey = \a b -> a { ckcProjConf = Just b }
    , cCheckKey = getProjConfAndModTime qe
    , cKeyValid = (==) `on` snd
    , cRegen = \(proj_conf, mtime) -> do
        shallowReconfigureProject qe
        readProjInfo qe proj_conf mtime pre_info
    }

getUnitInfo :: QueryEnv pt -> Unit pt -> IO UnitInfo
getUnitInfo qe@QueryEnv{..} unit@Unit{uDistDir} = do
  pre_info <- getPreInfo qe
  proj_info <- getProjInfo qe
  cached qe $ Cached
    { cGet = \c -> do
        ui <- Map.lookup uDistDir (qcUnitInfos c)
        return (uiModTimes ui, ui)
    , cSet = \c (_mtimes, unit_info) -> c { qcUnitInfos =
        Map.insert uDistDir unit_info (qcUnitInfos c) }

    , cGetKey = const Nothing
    , cSetKey = const
    , cCheckKey = getUnitModTimes unit
    , cKeyValid = (==)

    , cRegen = \mtimes -> do
        reconfigureUnit qe unit
        helper <- getHelper pre_info proj_info qe
        readUnitInfo helper unit mtimes
    }

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


-- | Regenerate project-level information by calling the appropriate build
-- system.
shallowReconfigureProject :: QueryEnvI (QCProgs a b) pt -> IO ()
shallowReconfigureProject QueryEnv
  { qeProjLoc = ProjLocStackYaml _stack_yaml, .. } = do
    -- Stack's dry-run only generates the cabal file from package.yaml (or
    -- well that's the only thing we would care about). reconfigureUnit
    -- will take care of this though and we don't need the cabal files
    -- before the Unit stage anyways.
    return ()
shallowReconfigureProject qe = do
  buildProjectTarget qe Nothing DryRun

reconfigureUnit :: QueryEnvI c pt -> Unit pt -> IO ()
reconfigureUnit qe u = buildProjectTarget qe (Just u) OnlyCfg

buildUnits :: [Unit pt] -> Query pt ()
buildUnits units = Query $ \qe -> do
  conf_progs <- getConfProgs qe
  forM_ units $ \u ->
    buildProjectTarget qe { qePrograms = conf_progs } (Just u) DoBuild

buildProject :: Query pt ()
buildProject = Query $ \qe -> do
  conf_progs <- getConfProgs qe
  buildProjectTarget qe { qePrograms = conf_progs } Nothing DoBuild

data BuildStage = DryRun | OnlyCfg | DoBuild

buildProjectTarget
    :: QueryEnvI c pt -> Maybe (Unit pt) -> BuildStage -> IO ()
buildProjectTarget qe mu stage = do
  -- Stack and cabal just happen to have the same stage options, totally by
  -- accident :)
  stage_opts :: [String] <- return $ case stage of
    DryRun  -> ["--dry-run"]
    OnlyCfg -> ["--only-configure"]
    DoBuild -> []
  -- TODO: version check for cabal's --only-configure
  case qe of
    QueryEnv { qeDistDir = DistDirCabal cpt distdir, qeProjLoc } -> do
      let projdir = plCabalProjectDir qeProjLoc
      cmd <- return $ case stage of
        DryRun | SCV1 <- cpt ->
          CabalInstall.CIConfigure
        OnlyCfg ->
          CabalInstall.CIConfigure
        _ ->
          CabalInstall.CIBuild
      CabalInstall.callCabalInstallCmd qe (Just projdir) cmd $
        case cpt of
          SCV1 ->
            [ "--builddir="++distdir ]
          SCV2 -> do
            targets <- return $ case mu of
              Nothing -> ["all"]
              Just Unit{uImpl} -> concat
                [ if uiV2OnlyDependencies uImpl
                    then ["--only-dependencies"] else []
                , uiV2Components uImpl
                ]
            case qeProjLoc of
              ProjLocV2File {plCabalProjectFile} ->
                [ "--project-file="++plCabalProjectFile
                , "--builddir="++distdir
                ] ++ stage_opts ++ targets
              ProjLocV2Dir {} ->
                [ "--builddir="++distdir
                ] ++ stage_opts ++ targets

    QueryEnv { qeDistDir = DistDirStack mworkdir
             , qeProjLoc = qeProjLoc@ProjLocStackYaml {plStackYaml}
             } -> do
      let projdir = plStackProjectDir qeProjLoc
      let workdir_opts = Stack.workdirArg qe
      case mu of
        Just Unit{uPackage=Package{pSourceDir}} ->
          Stack.callStackCmd qe (Just pSourceDir) $
            workdir_opts ++
            [ "--stack-yaml="++plStackYaml, "build", "."
            ] ++ stage_opts
        Nothing ->
          Stack.callStackCmd qe (Just projdir) $
            workdir_opts ++
            [ "--stack-yaml="++plStackYaml, "build"
            ] ++ stage_opts

getFileModTime :: FilePath -> IO (FilePath, EpochTime)
getFileModTime f = do
  t <- modificationTime <$> getFileStatus f
  return (f, t)

readProjInfo
    :: QueryEnvI c pt -> ProjConf pt -> ProjConfModTimes -> PreInfo pt -> IO (ProjInfo pt)
readProjInfo qe pc pcm pi = withVerbosity $ do
  let projloc = qeProjLoc qe
  case (qeDistDir qe, pc) of
    (DistDirCabal SCV1 distdir, ProjConfV1{pcV1CabalFile}) -> do
      setup_config_path <- canonicalizePath (distdir </> "setup-config")
      mhdr <- readSetupConfigHeader setup_config_path
      case mhdr of
        Just hdr@(UnitHeader (pkg_name_bs, _pkg_ver) ("Cabal", hdrCabalVersion) _compId) -> do
          let
            v3_0_0_0 = makeVersion [3,0,0,0]
            pkg_name
              | hdrCabalVersion >= v3_0_0_0 = BSU.toString pkg_name_bs
              | otherwise = BS8.unpack pkg_name_bs
            pkg = Package
              { pPackageName = pkg_name
              , pSourceDir = plCabalProjectDir projloc
              , pCabalFile = CabalFile pcV1CabalFile
              , pFlags = []
              , pUnits = (:|[]) Unit
                { uUnitId = UnitId pkg_name
                , uPackage = pkg { pUnits = () }
                , uDistDir = DistDirLib distdir
                , uImpl = UnitImplV1
                }
              }
            piImpl = ProjInfoV1 { piV1SetupHeader = hdr }
          return ProjInfo
            { piCabalVersion = hdrCabalVersion
            , piProjConfModTimes = pcm
            , piPackages = pkg :| []
            , piImpl
            }
        Just UnitHeader {uhSetupId=(setup_name, _)} ->
          panicIO $ printf "Unknown Setup package-id in setup-config header '%s': '%s'"
                      (BS8.unpack setup_name) setup_config_path
        Nothing ->
          panicIO $ printf "Could not read '%s' header" setup_config_path

    (DistDirCabal SCV2 distdirv2, _) -> do
      let plan_path = distdirv2 </> "cache" </> "plan.json"
      plan_mtime <- modificationTime <$> getFileStatus plan_path
      plan@PlanJson { pjCabalLibVersion=Ver pjCabalLibVersion
                    , pjCabalVersion
                    , pjCompilerId=PkgId (PkgName compName) (Ver compVer)
                    }
          <- decodePlanJson plan_path
      when (pjCabalVersion < Ver [2,4,1,0]) $
        panicIO $ "plan.json was produced by too-old a version of\
                  \cabal-install. The 'dist-dir' keys will be missing. \
                  \Please upgrade to at least cabal-instal-2.4.1.0"

      Just pkgs <- NonEmpty.nonEmpty <$> CabalInstall.planPackages plan
      return ProjInfo
        { piCabalVersion = makeDataVersion pjCabalLibVersion
        , piProjConfModTimes = pcm
        , piPackages = NonEmpty.sortWith pPackageName pkgs
        , piImpl = ProjInfoV2
          { piV2Plan = plan
          , piV2PlanModTime = plan_mtime
          , piV2CompilerId = (Text.unpack compName, makeDataVersion compVer)
          }
        }
    (DistDirStack{}, _) -> do
      Just cabal_files <- NonEmpty.nonEmpty <$> Stack.listPackageCabalFiles qe
      pkgs <- mapM (Stack.getPackage qe) cabal_files
      Just (cabalVer:_) <- runMaybeT $
        let ?progs = qePrograms qe in
        let PreInfoStack {piStackProjPaths} = pi in
        GHC.listCabalVersions (Just (sppGlobalPkgDb piStackProjPaths))
        --  ^ See [Note Stack Cabal Version]
      return ProjInfo
        { piCabalVersion = cabalVer
        , piProjConfModTimes = pcm
        , piPackages = NonEmpty.sortWith pPackageName pkgs
        , piImpl = ProjInfoStack
        }

-- [Note Stack Cabal Version]
--
-- Stack just uses ghc-pkg on the global-pkg-db to determine the
-- appropriate Cabal version for a resolver when building, see
-- Stack.Setup.pathsFromCompiler(cabalPkgVer). We do essentially the same
-- thing here.
--
-- The code for building Setup.hs is in Stack.Build.Execute and the version
-- of cabal is set in withSingleContext.withCabal.getPackageArgs.
--
-- Note there is some special casing going on (see 'depsMinusCabal'), they
-- use the packages from the snapshot pkg-db except Cabal which comes from
-- the global pkg-db.

readUnitInfo :: Helper pt -> Unit pt -> UnitModTimes -> IO UnitInfo
readUnitInfo helper unit@Unit {uUnitId=uiUnitId} uiModTimes = do
    res <- runHelper helper unit
           [ "package-id"
           , "compiler-id"
           , "flags"
           , "config-flags"
           , "non-default-config-flags"
           , "component-info"
           ]
    let [ Just (ChResponseVersion        uiPackageId),
          Just (ChResponseVersion        uiCompilerId),
          Just (ChResponseFlags          uiPackageFlags),
          Just (ChResponseFlags          uiConfigFlags),
          Just (ChResponseFlags          uiNonDefaultConfigFlags),
          Just (ChResponseComponentsInfo uiComponents)
          ] = res
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
    evaluate =<< qeReadProcess "" Nothing [] exe args1 `E.catch`
      \(_ :: E.IOException) ->
        panicIO $ concat
          ["invokeHelper", ": ", exe, " "
          , intercalate " " (map show args1)
          , " failed!"
          ]

-- | Make sure the appropriate helper executable for the given project is
-- installed and ready to run queries.
--
-- The idea is you can run this at a convinient time instead of having the
-- helper compilation happen during a time-sensitive user interaction. This
-- will however happen automatically as needed if you don't run it first.
prepare :: Query pt ()
prepare = Query $ \qe -> do
  pre_info <- getPreInfo qe
  proj_info <- getProjInfo qe
  void $ getHelper pre_info proj_info qe

-- | Create @cabal_macros.h@, @Paths_\<pkg\>.hs@ and other generated files
-- in the usual place. See 'Distribution.Simple.Build.initialBuildSteps'.
--
-- This is usually only needed on the first load of a unit or after the
-- cabal file changes.
writeAutogenFiles :: Unit pt -> Query pt ()
writeAutogenFiles unit = Query $ \qe -> do
  pre_info <- getPreInfo qe
  proj_info <- getProjInfo qe
  helper <- getHelper pre_info proj_info qe
  void $ runHelper helper unit ["write-autogen-files"]

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
  let ?verbose = \level ->
        case x >>= readMaybe of
          Just x | x >= level -> True
          _ -> False
  act

getConfProgs :: QueryEnvI (QCProgs a b) pt -> IO Programs
getConfProgs qe = do
  pre_info <- getPreInfo qe
  cached qe $ Cached
    { cGet = qcConfProgs
    , cSet = \a b -> a { qcConfProgs = Just b }
    , cGetKey = const Nothing
    , cSetKey = const
    , cCheckKey = return (qePrograms qe)
    , cKeyValid = (==)
    , cRegen = \_k -> configurePrograms qe pre_info
    }

-- | Fixup program paths as appropriate for current project-type and bring
-- 'Programs' into scope as an implicit parameter.
configurePrograms :: QueryEnvI c pt -> PreInfo pt -> IO Programs
configurePrograms qe@QueryEnv{..} pre_info = withVerbosity $ do
  patchBuildToolProgs (projTypeOfQueryEnv qe) <=< guessCompProgramPaths $
    case pre_info of
      PreInfoStack projPaths ->
        Stack.patchCompPrograms projPaths qePrograms
      _ -> qePrograms

newtype Helper pt
  = Helper { runHelper :: Unit pt -> [String] -> IO [Maybe ChResponse] }

getHelper :: PreInfo pt -> ProjInfo pt -> QueryEnvI c pt -> IO (Helper pt)
getHelper _pre_info ProjInfo{piCabalVersion} qe@QueryEnv{..}
  | piCabalVersion == bultinCabalVersion = return $ Helper $
      \Unit{ uDistDir=DistDirLib distdir
           , uPackage=Package{pCabalFile=CabalFile cabal_file}
           } args ->
        let pt = dispHelperProjectType (projTypeOfQueryEnv qe) in
        helper_main $ cabal_file : distdir : pt : args
getHelper pre_info proj_info qe@QueryEnv{..} = do
  withVerbosity $ do
    let ?progs = qePrograms
    t0 <- Clock.getTime Monotonic
    eexe <- compileHelper $ mkCompHelperEnv qeProjLoc qeDistDir pre_info proj_info
    t1 <- Clock.getTime Monotonic
    let dt = (/10^9) $ fromInteger $ Clock.toNanoSecs $ Clock.diffTimeSpec t0 t1
        dt :: Float
    vLog $ printf "compileHelper took %.5fs" dt
    case eexe of
      Left rv ->
        panicIO $ "compileHelper': compiling helper failed! exit code "++ show rv
      Right exe ->
        let pt = dispHelperProjectType (projTypeOfQueryEnv qe) in
        return $ Helper $ \Unit{uDistDir, uPackage=Package{pCabalFile}} args ->
          readHelper qe exe pCabalFile uDistDir (pt : args)

dispHelperProjectType :: SProjType pt -> String
dispHelperProjectType (SCabal SCV1) = "v1"
--  ^ v1-build needs a last minute addition of the inplace package-db
-- beyond what lbi has
dispHelperProjectType (SCabal SCV2) = "v2"
dispHelperProjectType SStack        = "v2"
--  ^ stack also embeds all necessary options into lbi like v2

mkCompHelperEnv
    :: Verbose
    => ProjLoc pt
    -> DistDir pt
    -> PreInfo pt
    -> ProjInfo pt
    -> CompHelperEnv
mkCompHelperEnv
  projloc
  (DistDirCabal SCV1 distdir)
  PreInfoCabal
  ProjInfo{piCabalVersion}
  = CompHelperEnv
    { cheCabalVer = CabalVersion piCabalVersion
    , cheProjDir  = plCabalProjectDir projloc
    , cheProjLocalCacheDir = distdir
    , chePkgDb    = Nothing
    , chePlanJson = Nothing
    , cheDistV2 = Nothing
    }
mkCompHelperEnv
  projloc
  (DistDirCabal SCV2 distdir)
  PreInfoCabal
  ProjInfo{piImpl=ProjInfoV2{piV2Plan=plan}}
  = CompHelperEnv {..}
  where
    cheProjDir  = plCabalProjectDir projloc
    cheCabalVer = CabalVersion $ makeDataVersion pjCabalLibVersion
    cheProjLocalCacheDir = distdir </> "cache"
    chePkgDb    = Nothing
    chePlanJson = Just plan
    cheDistV2   = Just distdir
    PlanJson {pjCabalLibVersion=Ver pjCabalLibVersion } = plan
mkCompHelperEnv
  (ProjLocStackYaml stack_yaml)
  (DistDirStack mworkdir)
  PreInfoStack
    { piStackProjPaths=StackProjPaths
      { sppGlobalPkgDb }
    }
  ProjInfo { piCabalVersion }
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

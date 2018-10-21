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
  -- * Running Queries
    Query
  , runQuery

  -- * Queries against Cabal\'s on disk state

  -- ** Project queries
  , compilerVersion
  , projectUnits

  -- ** Unit queries
  , Unit -- abstract
  , UnitId -- abstract
  , UnitInfo(..)
  , unitQuery

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

  -- * Reexports
  , module Data.Functor.Apply
  ) where

import Cabal.Plan hiding (Unit, UnitId, uDistDir)
import qualified Cabal.Plan as CP
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Exception as E
import Data.Coerce
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
import Data.Version
import Data.Function
import Data.Functor.Apply
import System.Environment
import System.FilePath hiding ((<.>))
import System.Directory
import System.Process
import System.Posix.Types
import System.PosixCompat.Files
import Text.Printf
import Text.Show.Pretty
import Prelude

import CabalHelper.Compiletime.Compile
import qualified CabalHelper.Compiletime.Program.Stack as Stack
import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.InterfaceTypes
import CabalHelper.Shared.Sandbox
import CabalHelper.Shared.Common

import CabalHelper.Compiletime.Compat.Version
import qualified CabalHelper.Compiletime.Compat.ProgramDb as ProgDb
    ( defaultProgramDb, programPath, lookupProgram, ghcProgram, ghcPkgProgram)
import CabalHelper.Shared.Common

import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent, deafening)
--import Distribution.Package (packageName, packageVersion)
import Distribution.Simple.GHC as GHC (configure)

import qualified CabalHelper.Compiletime.Compat.ProgramDb as ProgDb
    ( defaultProgramDb, programPath, lookupProgram, ghcProgram, ghcPkgProgram)
import CabalHelper.Compiletime.Compat.Version
import CabalHelper.Shared.Common

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
    { qeReadProcess = \mcwd exe args stdin ->
        readCreateProcess (proc exe args){ cwd = mcwd } stdin
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
  ProjConfV2
    { pcV2CabalProjFile       = projdir_path </> "cabal.project"
    , pcV2CabalProjLocalFile  = projdir_path </> "cabal.project.local"
    , pcV2CabalProjFreezeFile = projdir_path </> "cabal.project.freeze"
    }
projConf (ProjLocStackDir projdir_path) =
  ProjConfStack
    { pcStackYaml = projdir_path </> "stack.yml" }

getProjConfModTime :: ProjConf pt -> IO ProjConfModTimes
getProjConfModTime ProjConfV1{pcV1CabalFile} =
  fmap ProjConfModTimes $ mapM getFileModTime
    [ pcV1CabalFile
    ]
getProjConfModTime ProjConfV2{..} =
  fmap ProjConfModTimes $ mapM getFileModTime
    [ pcV2CabalProjFile
    , pcV2CabalProjLocalFile
    , pcV2CabalProjFreezeFile
    ]
getProjConfModTime ProjConfStack{..} =
  fmap ProjConfModTimes $ mapM getFileModTime
    [ pcStackYaml
    ]

getUnitModTimes :: Unit -> IO UnitModTimes
getUnitModTimes
  Unit
    { uDistDir=DistDirLib distdirv1
    , uCabalFile=CabalFile cabal_file_path
    }
  = do
    cabal_file_mtime <- getFileModTime cabal_file_path
    let setup_config = distdirv1 </> "setup-config"
    setup_config_mtime <- getFileModTime setup_config
    return UnitModTimes
      { umtCabalFile   = cabal_file_mtime
      , umtSetupConfig = setup_config_mtime
      }

-- | The version of GHC the project is configured to use
compilerVersion       :: Query pt (String, Version)
compilerVersion = undefined

-- | List of units in a project
projectUnits          :: Query pt [Unit]
projectUnits = Query $ \qe -> piUnits <$> getProjInfo qe

-- | Run a 'UnitQuery' on a given unit. To get a a unit see 'projectUnits'.
unitQuery          :: Unit -> Query pt UnitInfo
unitQuery u = Query $ \qe -> getUnitInfo qe u

-- | Get information on all units in a project.
allUnits :: (UnitInfo -> a) -> Query pt [a]
allUnits f = map f <$> (mapM unitQuery =<< projectUnits)

getProjInfo :: QueryEnv pt -> IO (ProjInfo pt)
getProjInfo qe@QueryEnv{..} = do
  cache@QueryCache{qcProjInfo, qcUnitInfos} <- readIORef qeCacheRef
  proj_info <- checkUpdateProjInfo qe qcProjInfo
  let active_units = piUnits proj_info
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

getUnitInfo :: QueryEnv pt -> Unit -> IO UnitInfo
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
    -> Unit
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
    :: [Unit]
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
  { qeProjLoc = ProjLocV2Dir projdir
  , qeDistDir = DistDirV2 _distdirv2, .. } = do
    _ <- liftIO $ qeReadProcess (Just projdir) (cabalProgram qePrograms)
           ["v2-build", "--dry-run", "all"] ""
    return ()
shallowReconfigureProject QueryEnv
  { qeProjLoc = ProjLocStackDir _projdir, .. } =
    -- TODO: do we need to do anything here? Maybe package.yaml support needs to
    -- do stuff here?
    return ()

reconfigureUnit :: QueryEnvI c pt -> Unit -> IO ()
reconfigureUnit QueryEnv{qeDistDir=DistDirV1{}, ..} Unit{uPackageDir=_} = do
  return ()
reconfigureUnit QueryEnv{qeDistDir=DistDirV2{}, ..} Unit{uPackageDir=_} = do
  return ()
reconfigureUnit QueryEnv{qeDistDir=DistDirStack{}, ..} Unit{uPackageDir} = do
  _ <- liftIO $ qeReadProcess (Just uPackageDir) (stackProgram qePrograms)
         ["stack", "build", "--only-configure", "."] ""
  return ()

findCabalFile :: FilePath -> IO FilePath
findCabalFile pkgdir = do
    [cfile] <- filter isCabalFile <$> getDirectoryContents pkgdir
    return cfile
  where
    isCabalFile :: FilePath -> Bool
    isCabalFile f = takeExtension' f == ".cabal"

    takeExtension' :: FilePath -> String
    takeExtension' p =
        if takeFileName p == takeExtension p
          then "" -- just ".cabal" is not a valid cabal file
          else takeExtension p

getFileModTime :: FilePath -> IO (FilePath, EpochTime)
getFileModTime f = do
  t <- modificationTime <$> getFileStatus f
  return (f, t)

readProjInfo
    :: QueryEnvI c pt -> ProjConf pt -> ProjConfModTimes -> IO (ProjInfo pt)
readProjInfo qe pc pcm = withVerbosity $ do
  case (qeProjLoc qe, qeDistDir qe, pc) of
    ((,,)
     projloc
     (DistDirV1 distdir)
     ProjConfV1{pcV1CabalFile}) -> do
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
            , piUnits = (:[]) $ Unit
              { uUnitId = UnitId ""
              , uPackageDir = projdir
              , uCabalFile = CabalFile pcV1CabalFile
              , uDistDir = DistDirLib distdir
              }
            , piImpl = ProjInfoV1
            }
    (ProjLocV2Dir _projdir, DistDirV2 distdirv2, _) -> do
      let plan_path = distdirv2 </> "cache" </> "plan.json"
      plan_mtime <- modificationTime <$> getFileStatus plan_path
      plan@PlanJson { pjCabalLibVersion=Ver pjCabalLibVersion
                    , pjCompilerId=PkgId (PkgName compName) (Ver compVer)
                    }
          <- decodePlanJson plan_path
      units <- planUnits plan
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
    (ProjLocStackDir{} , DistDirStack{}, _) -> do
      cabal_files <- Stack.listPackageCabalFiles qe
      units <- mapM (Stack.getUnit qe) cabal_files
      proj_paths <- Stack.projPaths qe
      cprogs <-
        guessCompProgramPaths $
        Stack.patchCompPrograms proj_paths $
        qeCompPrograms qe
      Just (cabalVer:_) <- runMaybeT $
        let ?cprogs = cprogs in
        let ?progs  = qePrograms qe in
        listCabalVersions' (Just (sppGlobalPkgDb proj_paths))
      -- ^ See [Note Stack Cabal Version]
      return ProjInfo
        { piCabalVersion = cabalVer
        , piProjConfModTimes = pcm
        , piUnits = units
        , piImpl = ProjInfoStack
          { piStackProjPaths = proj_paths
          }
        }

planUnits :: CP.PlanJson -> IO [Unit]
planUnits plan = do
    units <- fmap catMaybes $ mapM takeunit $ Map.elems $ CP.pjUnits plan
    case lefts units of
      [] -> return $ rights units
      us@(_:_) -> panicIO $
        msg ++ (concat $ map (unlines . map ("  "++) . lines . ppShow) us)
  where
    msg = "\
\plan.json doesn't contain 'dist-dir' key for the following local units:\n"
    takeunit u@CP.Unit
      { uType=CP.UnitTypeLocal
      , uDistDir=Just distdirv1
      , uPkgSrc=Just (CP.LocalUnpackedPackage pkgdir)
      } = do
        cabal_file <- findCabalFile pkgdir
        return $ Just $ Right $ Unit
          { uUnitId     = UnitId $ Text.unpack (coerce (CP.uId u))
          , uPackageDir = pkgdir
          , uCabalFile  = CabalFile cabal_file
          , uDistDir    = DistDirLib distdirv1
          }
    takeunit u@CP.Unit {uType=CP.UnitTypeLocal} =
      return $ Just $ Left u
    takeunit _ =
      return $ Nothing

readUnitInfo :: QueryEnvI c pt -> FilePath -> Unit -> IO UnitInfo
readUnitInfo
  qe exe unit@Unit {uUnitId=uiUnitId, uCabalFile, uDistDir} = do
    res <- readHelper qe exe uCabalFile uDistDir
           [ "package-id"
           , "package-db-stack"
           , "flags"
           , "compiler-version"
           , "config-flags"
           , "non-default-config-flags"
           , "component-info"
           ]
    let [ Just (ChResponseVersion        uiPackageId),
          Just (ChResponsePkgDbs         uiPackageDbStack),
          Just (ChResponseFlags          uiPackageFlags),
          Just (ChResponseVersion        uiCompilerVersion),
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
    evaluate =<< qeReadProcess Nothing exe args1 "" `E.catch`
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
writeAutogenFiles :: Unit -> Query pt ()
writeAutogenFiles Unit{uCabalFile, uDistDir} = Query $ \qe -> do
  proj_info <- getProjInfo qe
  exe <- getHelperExe proj_info qe
  void $ invokeHelper qe exe uCabalFile uDistDir ["write-autogen-files"]

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

withVerbosity :: (Verbose => IO a) -> IO a
withVerbosity act = do
  x <- lookup  "CABAL_HELPER_DEBUG" <$> getEnvironment
  let ?verbose =
        case x of
          Just xs | not (null xs) -> True
          _ -> False
  act

getHelperExe
    :: ProjInfo pt -> QueryEnvI c pt -> IO FilePath
getHelperExe proj_info QueryEnv{..} = do
  withVerbosity $ do
    let comp = wrapper' qeProjLoc qeDistDir proj_info
    let ?progs = qePrograms
        ?cprogs = qeCompPrograms
    eexe <- compileHelper comp
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
    { cheCabalVer = piCabalVersion
    , cheProjDir  = plV1Dir projloc
    , cheCacheDir = distdir
    , chePkgDb    = Nothing
    , cheNewstyle = Nothing
    }
wrapper'
  (ProjLocV2Dir projdir)
  (DistDirV2 distdir)
  ProjInfo{piImpl=ProjInfoV2{piV2Plan=plan}}
  = CompHelperEnv
    { cheCabalVer = makeDataVersion pjCabalLibVersion
    , cheProjDir  = projdir
    , cheCacheDir = distdir </> "cache"
    , chePkgDb    = Nothing
    , cheNewstyle = Just (plan, distdir)
    }
  where
    PlanJson {pjCabalLibVersion=Ver pjCabalLibVersion } = plan
wrapper'
  (ProjLocStackDir projdir)
  (DistDirStack mworkdir)
  ProjInfo
    { piCabalVersion
    , piImpl = ProjInfoStack
      { piStackProjPaths=StackProjPaths
        { sppGlobalPkgDb }
      }
    }
  = let workdir = fromMaybe ".stack-work" $ unRelativePath <$> mworkdir in
    CompHelperEnv
    { cheCabalVer = piCabalVersion
    , cheProjDir  = projdir
    , cheCacheDir = projdir </> workdir
    , chePkgDb    = Just sppGlobalPkgDb
    , cheNewstyle = Nothing
    }

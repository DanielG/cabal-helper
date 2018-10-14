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

  -- * Query environment
  , QueryEnv -- abstract
  , mkQueryEnv
  , qeReadProcess
  , qePrograms
  , qeProjectDir
  , qeDistDir

  -- * GADTs
  , DistDir(..)
  , ProjType(..)
  , ProjDir(..)

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
  , reconfigure
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
import Data.Char
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
import CabalHelper.Compiletime.Types
import CabalHelper.Shared.InterfaceTypes
import CabalHelper.Shared.Sandbox
import CabalHelper.Shared.Common

import CabalHelper.Compiletime.Compat.Version
import qualified CabalHelper.Compiletime.Compat.ProgramDb as ProgDb
    ( defaultProgramDb, programPath, lookupProgram, ghcProgram, ghcPkgProgram)

import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent, deafening)
--import Distribution.Package (packageName, packageVersion)
import Distribution.Simple.GHC as GHC (configure)

import qualified CabalHelper.Compiletime.Compat.ProgramDb as ProgDb
    ( defaultProgramDb, programPath, lookupProgram, ghcProgram, ghcPkgProgram)
import CabalHelper.Compiletime.Compat.Version
import CabalHelper.Shared.Common


-- | The kind of project being managed by a 'QueryEnv' (pun intended).
data ProjType
    = V1    -- ^ @cabal v1-build@ project, see 'DistDirV1'
    | V2    -- ^ @cabal v2-build@ project, see 'DistDirV2'

-- | A project directory. The project type of a given directory can be
-- determined by trying to access a set of marker files. See below.
data ProjDir (pt :: ProjType) where
    -- | A @cabal v1-build@ project directory can be identified by one file
    -- ending in @.cabal@ existing in the directory. More than one such files
    -- existing is a user error. Note: For this project type the concepts of
    -- project and package coincide.
    ProjDirV1    :: FilePath -> ProjDir 'V1

    -- | A @cabal v2-build@ project\'s marker file is called
    -- @cabal.project@. This configuration file points to the packages that make
    -- up this project.
    ProjDirV2    :: FilePath -> ProjDir 'V2

data DistDir (pt :: ProjType) where
    -- | Build directory for cabal /old-build/ aka. /v1-build/ aka. just
    -- /build/. Planned to be superceeded by /v2-build/, see 'DistDirV2' for
    -- that.
    --
    -- You can tell a builddir is a /v1/ builddir by looking for a file
    -- called @setup-config@ directly underneath it.
    DistDirV1 :: FilePath -> DistDir 'V1

    -- | Build directory for cabal /new-build/ aka. /v2-build/, as of the time
    -- of this writing it is usually called @dist-newstyle/@ but this will
    -- presumably change once it becomes the default /build/ command.
    --
    -- You can tell a builddir is a /v2/ builddir by trying to access the path
    -- @cache/plan.json@ directly underneath it.
    DistDirV2 :: FilePath -> DistDir 'V2

-- | Environment for running a 'Query' value. The real constructor is
-- not exposed, use the 'mkQueryEnv' smart constructor instead. The field
-- accessors are exported and may be used to override the defaults, see below.
type QueryEnv (proj_type :: ProjType)
    = QueryEnvI (QueryCache proj_type) proj_type

data QueryEnvI cache (proj_type :: ProjType) = QueryEnv
    { qeReadProcess
          :: Maybe FilePath -> FilePath -> [String] -> String -> IO String
    -- ^ Field accessor for 'QueryEnv'. Function used to to start
    -- processes. Useful if you need to, for example, redirect standard error
    -- output away from the user\'s terminal.

    , qePrograms    :: Programs
    -- ^ Field accessor for 'QueryEnv'.

    , qeProjectDir  :: ProjDir proj_type
    -- ^ Field accessor for 'QueryEnv'. Defines path to the project directory,
    -- i.e. a directory containing a @cabal.project@ file

    , qeDistDir     :: DistDir proj_type
    -- ^ Field accessor for 'QueryEnv'. Defines path to the @dist/@ or
    -- @dist-newstyle/@ directory, aka. /builddir/ in Cabal terminology.

    , qeCacheRef    :: IORef cache
    -- ^ Cache for query results, only accessible when type parameter @cache@ is
    -- instantiated and not forall quantified.
    }

data QueryCache pt = QueryCache
    { qcProjInfo  :: !(Maybe (ProjInfo pt))
    , qcUnitInfos :: !(Map DistDirLib UnitInfo)
    }

newtype DistDirLib = DistDirLib FilePath
    deriving (Eq, Ord, Read, Show)

-- | Abstractly speaking a Unit consists of a set of components (exes, libs,
-- tests etc.) which are managed by an instance of the Cabal build system. The
-- distinction between a Unit and a set of components is somewhat hard to
-- explain if you're not already familliar with the concept from
-- cabal-install. Luckily for most purposes the details may be ignored.
--
-- We merely use the concept of a Unit for caching purposes. It is necessary to
-- extract the information on all components in a Unit at the same time as we
-- must load all of it into memory before extracting any of it.
--
-- As opposed to components, different 'Unit's can be queried independently
-- since their on-disk information is stored separately.
data Unit = Unit
    { uUnitId      :: !UnitId
    , uPackageDir  :: !FilePath
    , uDistDir     :: !DistDirLib
    }

newtype UnitId = UnitId String
    deriving (Eq, Ord, Read, Show)

-- | The information extracted from a 'Unit's on-disk configuration.
data UnitInfo = UnitInfo
    { uiUnitId                :: !UnitId
    -- ^ A unique identifier of this init within the project.

    , uiComponents            :: !(Map ChComponentName ChComponentInfo)
    -- ^ The components of the unit: libraries, executables, test-suites,
    -- benchmarks and so on.

    , uiCompilerVersion       :: !(String, Version)
    -- ^ The version of GHC the unit is configured to use

    , uiPackageDbStack        :: !([ChPkgDb])
    -- ^ List of package databases to use.

    , uiPackageFlags          :: !([(String, Bool)])
    -- ^ Flag definitions from cabal file

    , uiConfigFlags           :: ![(String, Bool)]
    -- ^ Flag assignments from active configuration

    , uiNonDefaultConfigFlags :: ![(String, Bool)]
    -- ^ Flag assignments from setup-config which differ from the default
    -- setting. This can also include flags which cabal decided to modify,
    -- i.e. don't rely on these being the flags set by the user directly.

    , uiModTimes              :: !UnitModTimes
    } deriving (Eq, Ord, Read, Show)

data ProjInfo pt where
  ProjInfoV1 ::
    { piV1ProjConfModTimes :: !(ProjConfModTimes 'V1)
    } -> ProjInfo 'V1

  ProjInfoV2 ::
    { piV2ProjConfModTimes :: !(ProjConfModTimes 'V2)
    , piV2Plan             :: !PlanJson
    , piV2PlanModTime      :: !EpochTime
    } -> ProjInfo 'V2

data ProjConfModTimes pt where
    ProjConfModTimesV1
        :: !(FilePath, EpochTime)     -> ProjConfModTimes 'V1
    ProjConfModTimesV2
        :: !([(FilePath, EpochTime)]) -> ProjConfModTimes 'V2

deriving instance Eq (ProjConfModTimes pt)

piProjConfModTimes :: ProjInfo pt -> ProjConfModTimes pt
piProjConfModTimes ProjInfoV1 {piV1ProjConfModTimes} =
    piV1ProjConfModTimes
piProjConfModTimes ProjInfoV2 {piV2ProjConfModTimes} =
    piV2ProjConfModTimes

data UnitModTimes = UnitModTimes
    { umtCabalFile   :: !(FilePath, EpochTime)
    , umtSetupConfig :: !(FilePath, EpochTime)
    } deriving (Eq, Ord, Read, Show)

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
-- Sets fields 'qeProjectDir' and 'qeDistDir' to @projdir@ and @distdir@
-- respectively and provides sensible defaults for the other fields.
mkQueryEnv
    :: ProjDir pt
    -- ^ Path to the project directory
    -> DistDir pt
    -- ^ Path to the @dist/@ or @dist-newstyle/@ directory, called
    -- /builddir/ in Cabal terminology.
    -> IO (QueryEnv pt)
mkQueryEnv projdir distdir = do
  cr <- newIORef $ QueryCache Nothing Map.empty
  return $ QueryEnv
    { qeReadProcess = \mcwd exe args stdin ->
        readCreateProcess (proc exe args){ cwd = mcwd } stdin
    , qePrograms    = defaultPrograms
    , qeProjectDir  = projdir
    , qeDistDir     = distdir
    , qeCacheRef    = cr
    }

piUnits :: DistDir pt -> ProjInfo pt -> [Unit]
piUnits (DistDirV1 distdir) (ProjInfoV1 (ProjConfModTimesV1 (cabal_file, _))) =
  (:[]) $ Unit
    { uUnitId = UnitId ""
    , uPackageDir = takeDirectory cabal_file
    , uDistDir = DistDirLib distdir
    }
piUnits _ ProjInfoV2{..} =
    case lefts units of
      [] -> rights units
      us@(_:_) -> panic $
        msg ++ (concat $ map (unlines . map ("  "++) . lines . ppShow) us)
  where
    msg = "\
\plan.json doesn't contain 'dist-dir' key for the following local units:\n"
    units = catMaybes $ map takeunit $ Map.elems $ pjUnits piV2Plan
    takeunit u@CP.Unit
      { uType=UnitTypeLocal
      , uDistDir=Just distdirv1
      , uPkgSrc=Just (LocalUnpackedPackage pkgdir)
      } = Just $ Right $ Unit
          { uUnitId     = UnitId $ Text.unpack (coerce (uId u))
          , uPackageDir = pkgdir
          , uDistDir    = DistDirLib distdirv1
          }
    takeunit u@CP.Unit {uType=UnitTypeLocal} =
      Just $ Left u
    takeunit _ =
      Nothing


-- | Find files relevant to the project-scope configuration. Depending on the
-- 'ProjType' this could be (for example) just a cabal file, one of the
-- @caba.project*@ files or @stack.yaml@.
--
-- The returned paths include the project-dir path.
projConfModTimes :: ProjDir pt -> IO (ProjConfModTimes pt)
projConfModTimes pd@(ProjDirV1 _) =
    ProjConfModTimesV1 <$> (getFileModTime =<< findCabalFile pd)
projConfModTimes (ProjDirV2 projdir) = do
    ex_files <- filterM doesFileExist (map (projdir </>) additional_files)
    let files = [ projdir </> "cabal.project" ] ++ ex_files
    ProjConfModTimesV2 <$> mapM getFileModTime files
  where
    additional_files =
        [ "cabal.project.local"
        , "cabal.project.freeze"
        ]

getUnitModTimes :: Unit -> IO UnitModTimes
getUnitModTimes Unit { uDistDir=DistDirLib distdirv1, uPackageDir=pkgdir } = do
  cabal_file <- findCabalFile (ProjDirV1 pkgdir)
  cabal_file_mtime <- getFileModTime cabal_file

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
projectUnits = Query $ \qe@QueryEnv{qeDistDir} ->
  piUnits qeDistDir <$> getProjInfo qe

-- | Run a 'UnitQuery' on a given unit. To get a a unit see 'projectUnits'.
unitQuery          :: Unit -> Query pt UnitInfo
unitQuery u = Query $ \qe -> getUnitInfo qe u



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


getProjInfo :: QueryEnv pt -> IO (ProjInfo pt)
getProjInfo qe@QueryEnv{..} = do
  cache@QueryCache{qcProjInfo, qcUnitInfos} <- readIORef qeCacheRef
  proj_info <- checkUpdateProj qe qcProjInfo
  let active_units = piUnits qeDistDir proj_info
  writeIORef qeCacheRef $ cache
    { qcProjInfo  = Just proj_info
    , qcUnitInfos = discardInactiveUnitInfos active_units qcUnitInfos
    }
  return proj_info

checkUpdateProj
    :: QueryEnvI c pt
    -> Maybe (ProjInfo pt)
    -> IO (ProjInfo pt)
checkUpdateProj qe mproj_info = do
  mtime <- projConfModTimes (qeProjectDir qe)

  case mproj_info of
    Nothing -> reconf mtime
    Just proj_info
        | piProjConfModTimes proj_info /= mtime
            -> reconf mtime
        | otherwise
            -> return proj_info
  where
    reconf mtime = do
      shallowReconfigureProject qe
      readProjInfo qe mtime



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
      helper <- wrapper proj_info qe
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
  { qeProjectDir = ProjDirV1 _projdir
  , qeDistDir = DistDirV1 _distdirv1 } =
    return ()
shallowReconfigureProject QueryEnv
  { qeProjectDir = ProjDirV2 projdir
  , qeDistDir = DistDirV2 _distdirv2, .. } = do
    _ <- liftIO $ qeReadProcess (Just projdir) (cabalProgram qePrograms)
           ["v2-build", "--dry-run", "all"] ""
    return ()

reconfigureUnit :: QueryEnvI c pt -> Unit -> IO ()
reconfigureUnit QueryEnv{qeDistDir=DistDirV1{}, ..} Unit{uPackageDir=_} = do
  return ()
reconfigureUnit QueryEnv{qeDistDir=DistDirV2{}, ..} Unit{uPackageDir=_} = do
  return ()

findCabalFile :: ProjDir 'V1 -> IO FilePath
findCabalFile (ProjDirV1 pkgdir) = do
  [cfile] <- filter isCabalFile <$> getDirectoryContents pkgdir
  return cfile

getFileModTime :: FilePath -> IO (FilePath, EpochTime)
getFileModTime f = do
  t <- modificationTime <$> getFileStatus f
  return (f, t)

readProjInfo :: QueryEnvI c pt -> ProjConfModTimes pt -> IO (ProjInfo pt)
readProjInfo qe conf_files = do
  case (qeProjectDir qe, qeDistDir qe) of
    (ProjDirV1 _projdir, DistDirV1 _) ->
      return $ ProjInfoV1 { piV1ProjConfModTimes = conf_files }
    (ProjDirV2 _projdir, DistDirV2 distdirv2) -> do
      let plan_path = distdirv2 </> "cache" </> "plan.json"
      plan_mtime <- modificationTime <$> getFileStatus plan_path
      plan <- decodePlanJson plan_path
      return $ ProjInfoV2
        { piV2ProjConfModTimes = conf_files
        , piV2Plan = plan
        , piV2PlanModTime = plan_mtime
        }

readUnitInfo :: QueryEnvI c pt -> FilePath -> Unit -> IO UnitInfo
readUnitInfo
  qe exe unit@Unit {uUnitId=uiUnitId, uPackageDir=pkgdir, uDistDir=distdir} = do
    res <- readHelper qe exe pkgdir distdir
           [ "package-db-stack"
           , "flags"
           , "compiler-version"
           , "config-flags"
           , "non-default-config-flags"
           , "component-info"
           ]
    let [ Just (ChResponsePkgDbs         uiPackageDbStack),
          Just (ChResponseFlags          uiPackageFlags),
          Just (ChResponseVersion        comp compVer),
          Just (ChResponseFlags          uiConfigFlags),
          Just (ChResponseFlags          uiNonDefaultConfigFlags),
          Just (ChResponseComponentsInfo uiComponents)
          ] = res
        uiCompilerVersion = (comp, compVer)
    uiModTimes <- getUnitModTimes unit
    return $ UnitInfo {..}

readHelper
    :: QueryEnvI c pt
    -> FilePath
    -> FilePath
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
    -> FilePath
    -> DistDirLib
    -> [String]
    -> IO String
invokeHelper QueryEnv {..} exe cabal_file (DistDirLib distdir) args0 = do
  let args1 = cabal_file : distdir : args0
  evaluate =<< qeReadProcess Nothing exe args1 "" `E.catch`
    \(_ :: E.IOException) ->
      panicIO $ concat
        ["invokeHelper", ": ", exe, " "
        , intercalate " " (map show args1)
        , " failed!"
        ]

-- getPackageId :: QueryEnv pt -> IO (String, Version)
-- getPackageId QueryEnv{..}  = do
--   [cfile] <- filter isCabalFile <$> getDirectoryContents qeProjectDir
--   gpd <- readPackageDescription silent (qeProjectDir </> cfile)
--   return $ (display (packageName gpd), toDataVersion (packageVersion gpd))

-- | Make sure the appropriate helper executable for the given project is
-- installed and ready to run queries.
prepare :: QueryEnv pt -> IO ()
prepare qe = do
  proj_info <- getProjInfo qe
  void $ wrapper proj_info qe

-- | Create @cabal_macros.h@ and @Paths_\<pkg\>@ possibly other generated files
-- in the usual place.
writeAutogenFiles :: QueryEnv pt -> IO ()
writeAutogenFiles qe = do
  proj_info <- getProjInfo qe
  _exe <- wrapper proj_info qe
  undefined -- void $ invokeHelper qe exe ["write-autogen-files"]

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

guessProgramPaths :: (Verbose, Progs) => (Progs => IO a) -> IO a
guessProgramPaths act = do
    let v | ?verbose  = deafening
          | otherwise = silent

        mGhcPath0    | same ghcProgram ?progs dprogs = Nothing
                     | otherwise = Just $ ghcProgram ?progs
        mGhcPkgPath0 | same ghcPkgProgram ?progs dprogs = Nothing
                     | otherwise = Just $ ghcPkgProgram ?progs

    (_compiler, _mplatform, progdb)
        <- GHC.configure
               v
               mGhcPath0
               mGhcPkgPath0
               ProgDb.defaultProgramDb
    let getProg p = ProgDb.programPath <$> ProgDb.lookupProgram p progdb
        mghcPath1    = getProg ProgDb.ghcProgram
        mghcPkgPath1 = getProg ProgDb.ghcPkgProgram

    let ?progs = ?progs
          { ghcProgram    = fromMaybe (ghcProgram ?progs) mghcPath1
          , ghcPkgProgram = fromMaybe (ghcProgram ?progs) mghcPkgPath1
          }
    act
 where
   same f o o'  = f o == f o'
   dprogs = defaultPrograms

withVerbosity :: (Verbose => a) -> IO a
withVerbosity a = do
  x <- lookup  "CABAL_HELPER_DEBUG" <$> getEnvironment
  let ?verbose =
        case x of
          Just xs | not (null xs) -> True
          _ -> False
  return a

wrapper
    :: ProjInfo pt -> QueryEnvI c pt -> IO FilePath
wrapper proj_info QueryEnv{..} = do
  join $ withVerbosity $ do
    let ?progs = qePrograms
    guessProgramPaths $ wrapper' qeProjectDir qeDistDir proj_info

wrapper'
    :: Env
    => ProjDir pt
    -> DistDir pt
    -> ProjInfo pt
    -> IO FilePath
wrapper' (ProjDirV1 projdir) (DistDirV1 distdir) _ = do
  cfgf <- canonicalizePath (distdir </> "setup-config")
  mhdr <- getCabalConfigHeader cfgf
  case mhdr of
    Nothing -> panicIO $ printf "\
\Could not read Cabal's persistent setup configuration header\n\
\- Check first line of: %s\n\
\- Maybe try: $ cabal configure" cfgf
    Just (hdrCabalVersion, _) -> do
      compileHelper' hdrCabalVersion Nothing projdir Nothing distdir
wrapper'
  (ProjDirV2 projdir)
  (DistDirV2 distdir)
  ProjInfoV2{piV2Plan=plan}
  = do
    let PlanJson {pjCabalLibVersion=Ver (makeDataVersion -> pjCabalLibVersion) }
          = plan
    compileHelper' pjCabalLibVersion
                   Nothing
                   projdir
                   (Just (plan, distdir))
                   (distdir </> "cache")

compileHelper'
    :: Env
    => Version
    -> Maybe PackageDbDir
    -> FilePath
    -> Maybe (PlanJson, FilePath)
    -> FilePath
    -> IO FilePath
compileHelper' pjCabalLibVersion cabalPkgDb projdir mnewstyle distdirv1 = do
  eexe <- compileHelper pjCabalLibVersion cabalPkgDb projdir mnewstyle distdirv1
  case eexe of
    Left rv ->
      panicIO $ "compileHelper': compiling helper failed! (exit code "++ show rv
    Right exe ->
      return exe

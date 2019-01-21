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

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DefaultSignatures,
  StandaloneDeriving, GADTs, DataKinds, KindSignatures, RankNTypes #-}

{-|
Module      : CabalHelper.Compiletime.Types
Description : Types used throughout
License     : GPL-3
-}

module CabalHelper.Compiletime.Types where

import Cabal.Plan
  ( PlanJson )
import Data.IORef
import Data.Version
import Data.Typeable
import GHC.Generics
import System.FilePath
import System.Posix.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.InterfaceTypes

import Data.List.NonEmpty (NonEmpty)
--import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
--import qualified Data.Map.Strict as Strict

-- | The kind of project being managed by a 'QueryEnv' (pun intended).
data ProjType
    = V1    -- ^ @cabal v1-build@ project, see 'DistDirV1'
    | V2    -- ^ @cabal v2-build@ project, see 'DistDirV2'
    | Stack -- ^ @stack@ project.
      deriving (Eq, Ord, Show, Read)

data SProjType pt where
    SV1 :: SProjType 'V1
    SV2 :: SProjType 'V2
    SStack :: SProjType 'Stack

-- | Location of project sources. The project type of a given directory can be
-- determined by trying to access a set of marker files. See below.
data ProjLoc (pt :: ProjType) where
    -- | A @cabal v1-build@ project directory can be identified by one file
    -- ending in @.cabal@ existing in the directory. More than one such files
    -- existing is a user error. Note: For this project type the concepts of
    -- project and package coincide.
    ProjLocCabalFile :: { plCabalFile :: !FilePath } -> ProjLoc 'V1

    -- | A @cabal v2-build@ project\'s marker file is called
    -- @cabal.project@. This configuration file points to the packages that make
    -- up this project.
    ProjLocV2File    :: { plCabalProjectFile :: !FilePath } -> ProjLoc 'V2
    ProjLocV2Dir     :: { plV2Dir :: !FilePath } -> ProjLoc 'V2

    -- | A @stack@ project\'s marker file is called @stack.yaml@. This
    -- configuration file points to the packages that make up this project.
    ProjLocStackYaml :: { plStackYaml :: !FilePath } -> ProjLoc 'Stack

deriving instance Show (ProjLoc pt)

plV1Dir :: ProjLoc 'V1 -> FilePath
plV1Dir (ProjLocCabalFile cabal_file) = takeDirectory cabal_file

data DistDir (pt :: ProjType) where
    -- | Build directory for cabal /old-build/ aka. /v1-build/ aka. just
    -- /build/. Planned to be superceeded by /v2-build/, see 'DistDirV2' for
    -- that.
    --
    -- You can tell a builddir is a /v1/ builddir by looking for a file
    -- called @setup-config@ directly underneath it.
    DistDirV1 :: !FilePath -> DistDir 'V1

    -- | Build directory for cabal /new-build/ aka. /v2-build/, as of the time
    -- of this writing it is usually called @dist-newstyle/@ but this will
    -- presumably change once it becomes the default /build/ command.
    --
    -- You can tell a builddir is a /v2/ builddir by trying to access the path
    -- @cache/plan.json@ directly underneath it.
    DistDirV2 :: !FilePath -> DistDir 'V2

    -- | Build directory for stack, aka. /work-dir/. Optionally override Stack's
    -- /work-dir/. If you just want to use Stack's default set to @Nothing@
    DistDirStack :: !(Maybe RelativePath) -> DistDir 'Stack

deriving instance Show (DistDir pt)

-- | Environment for running a 'Query' value. The constructor is not exposed in
-- the API to allow extending the environment without breaking user code.
--
-- To create a 'QueryEnv' use the 'mkQueryEnv' smart constructor. The field
-- accessors are exported and may be used to override the defaults filled in by
-- 'mkQueryEnv'. See below.
type QueryEnv (pt :: ProjType)
    = QueryEnvI QueryCache pt

data QueryEnvI c (pt :: ProjType) = QueryEnv
    { qeReadProcess :: !ReadProcessWithCwd
    -- ^ Field accessor for 'QueryEnv'. Function used to to start
    -- processes. Useful if you need to, for example, redirect standard error
    -- output of programs started by cabal-helper.

    , qeCallProcess :: !(CallProcessWithCwd ())

    , qePrograms     :: !Programs
    -- ^ Field accessor for 'QueryEnv'.

    , qeCompPrograms :: !CompPrograms
    -- ^ Field accessor for 'QueryEnv'.

    , qeProjLoc      :: !(ProjLoc pt)
    -- ^ Field accessor for 'QueryEnv'. Defines path to the project directory,
    -- i.e. a directory containing a @cabal.project@ file

    , qeDistDir      :: !(DistDir pt)
    -- ^ Field accessor for 'QueryEnv'. Defines path to the @dist/@ or
    -- @dist-newstyle/@ directory, aka. /builddir/ in Cabal terminology.

    , qeCacheRef     :: !(IORef (c pt))
    -- ^ Cache for query results, only accessible when type parameter @c@ is
    -- instantiated with 'QueryCache'. This is the case wherever the type alias
    -- 'QueryEnv' is used.
    }

type ReadProcessWithCwd   = String -> CallProcessWithCwd String
type CallProcessWithCwd a = Maybe FilePath -> FilePath -> [String] -> IO a

data QueryCache pt = QueryCache
    { qcProjInfo  :: !(Maybe (ProjInfo pt))
    , qcUnitInfos :: !(Map DistDirLib UnitInfo)
    }

newtype DistDirLib = DistDirLib FilePath
    deriving (Eq, Ord, Read, Show)

-- | A 'Unit' is used as reference to a set of components (exes, libs, tests
-- etc.) which are managed by an certain instance of the Cabal build system. We
-- may get information on the components in a unit by retriving the
-- corresponding 'UnitInfo'.
data Unit pt = Unit
    { uUnitId      :: !UnitId
    , uPackageDir  :: !FilePath
    , uCabalFile   :: !CabalFile
    , uDistDir     :: !DistDirLib
    , uImpl        :: !(UnitImpl pt)
    } deriving (Show)

data UnitImpl pt where
  UnitImplV1 :: UnitImpl 'V1

  UnitImplV2 ::
    { uiV2ComponentNames :: ![ChComponentName]
    , uiV2Components     :: ![String]
    } -> UnitImpl 'V2

  UnitImplStack :: UnitImpl 'Stack

deriving instance Show (UnitImpl pt)

-- | This returns the component a 'Unit' corresponds to. This information is
-- only available if the correspondence happens to be unique and known before
-- querying setup-config for the respective project type. Currently this only
-- applies to @pt=@'V2'.
--
-- This is intended to be used as an optimization, to allow reducing the number
-- of helper invocations for clients that don't need to know the entire project
-- structure.
uComponentName :: Unit pt -> Maybe ChComponentName
uComponentName Unit { uImpl=UnitImplV2 { uiV2ComponentNames=[comp] } } =
    Just comp
uComponentName _ =
    Nothing

newtype UnitId = UnitId String
    deriving (Eq, Ord, Read, Show)

-- | The information extracted from a 'Unit'\'s on-disk configuration cache.
data UnitInfo = UnitInfo
    { uiUnitId                :: !UnitId
    -- ^ A unique identifier of this unit within the project.

    , uiPackageId             :: !(String, Version)
    -- ^ The package-name and version this unit belongs to.

    , uiComponents            :: !(Map ChComponentName ChComponentInfo)
    -- ^ The components of the unit: libraries, executables, test-suites,
    -- benchmarks and so on.

    , uiCompilerId            :: !(String, Version)
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
    -- ^ Key for cache invalidation. When this is not equal to the value
    -- returned by 'getUnitModTimes' this 'UnitInfo' is considered invalid.
    } deriving (Eq, Ord, Read, Show)

-- | Files relevant to the project-scope configuration. We gather them here so
-- we can refer to their paths conveniently throughout the code. These files are
-- not necessarily guaranteed to even exist.
data ProjConf pt where
  ProjConfV1 ::
    { pcV1CabalFile :: !FilePath
    } -> ProjConf 'V1

  ProjConfV2 ::
    { pcV2CabalProjFile       :: !FilePath
    , pcV2CabalProjLocalFile  :: !FilePath
    , pcV2CabalProjFreezeFile :: !FilePath
    } -> ProjConf 'V2

  ProjConfStack ::
    { pcStackYaml :: !FilePath
    } -> ProjConf 'Stack

-- these are supposed to be opaque, as they are meant to be used only for cache
-- invalidation
newtype ProjConfModTimes = ProjConfModTimes [(FilePath, EpochTime)]
    deriving (Eq, Show)

-- | Project-scope information cache.
data ProjInfo pt = ProjInfo
  { piCabalVersion     :: !Version
  , piUnits            :: !(NonEmpty (Unit pt))
  , piImpl             :: !(ProjInfoImpl pt)
  , piProjConfModTimes :: !ProjConfModTimes
  -- ^ Key for cache invalidation. When this is not equal to the return
  -- value of 'getProjConfModTime' this 'ProjInfo' is considered invalid.
  } deriving (Show)

data ProjInfoImpl pt where
  ProjInfoV1 :: ProjInfoImpl 'V1

  ProjInfoV2 ::
    { piV2Plan        :: !PlanJson
    , piV2PlanModTime :: !EpochTime
    , piV2CompilerId  :: !(String, Version)
    } -> ProjInfoImpl 'V2

  ProjInfoStack ::
    { piStackProjPaths :: !StackProjPaths
    } -> ProjInfoImpl 'Stack

instance Show (ProjInfoImpl pt) where
    show ProjInfoV1 = "ProjInfoV1"
    show ProjInfoV2 {..} = concat
      [ "ProjInfoV2 {"
      , "piV2Plan = ", show piV2Plan, ", " --
      , "piV2PlanModTime = ", show piV2PlanModTime, ", "
      , "piV2CompilerId = ", show piV2CompilerId
      , "}"
      ]
    show ProjInfoStack {..} = concat
      [ "ProjInfoStack {"
      , "piStackProjPaths = ", show piStackProjPaths
      , "}"
      ]

data UnitModTimes = UnitModTimes
    { umtPkgYaml     :: !(Maybe (FilePath, EpochTime))
    , umtCabalFile   :: !(FilePath, EpochTime)
    , umtSetupConfig :: !(Maybe (FilePath, EpochTime))
    } deriving (Eq, Ord, Read, Show)

newtype CabalFile = CabalFile FilePath
    deriving (Show)

data StackProjPaths = StackProjPaths
    { sppGlobalPkgDb :: !PackageDbDir
    , sppSnapPkgDb   :: !PackageDbDir
    , sppLocalPkgDb  :: !PackageDbDir
    , sppCompExe     :: !FilePath
    } deriving (Show)


-- Beware: GHC 8.0.2 doesn't like these being recursively defined for some
-- reason so just keep them unrolled.
type Verbose = (?verbose :: Bool)
type Env     = (?cprogs :: CompPrograms, ?progs :: Programs, ?verbose :: Bool)
type Progs   = (?cprogs :: CompPrograms, ?progs :: Programs)
type CProgs  = (?cprogs :: CompPrograms)

-- | Configurable paths to various programs we use.
data Programs = Programs
    { cabalProgram    :: !FilePath
      -- ^ The path to the @cabal@ program.
    , cabalArgsBefore :: ![String]
    , cabalArgsAfter  :: ![String]

    , stackProgram    :: !FilePath
      -- ^ The path to the @stack@ program.
    , stackArgsBefore :: ![String]
    , stackArgsAfter  :: ![String]
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

data CompPrograms = CompPrograms
    { ghcProgram    :: !FilePath
    -- ^ The path to the @ghc@ program.

    , ghcPkgProgram :: !FilePath
    -- ^ The path to the @ghc-pkg@ program. If not changed it will be derived
    -- from the path to 'ghcProgram'.
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

-- | By default all programs use their unqualified names, i.e. they will be
-- searched for on @PATH@.
defaultPrograms :: Programs
defaultPrograms = Programs "cabal" [] []  "stack" [] []

defaultCompPrograms :: CompPrograms
defaultCompPrograms = CompPrograms "ghc" "ghc-pkg"

data CompileOptions = CompileOptions
    { oVerbose       :: Bool
    , oCabalPkgDb    :: Maybe PackageDbDir
    , oCabalVersion  :: Maybe Version
    , oPrograms      :: Programs
    }

oCabalProgram :: Env => FilePath
oCabalProgram = cabalProgram ?progs

defaultCompileOptions :: CompileOptions
defaultCompileOptions =
    CompileOptions False Nothing Nothing defaultPrograms

newtype PackageDbDir = PackageDbDir { unPackageDbDir :: FilePath }
    deriving (Show)
newtype PackageEnvFile = PackageEnvFile { unPackageEnvFile :: FilePath }
    deriving (Show)

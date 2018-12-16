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
import Data.Map.Strict (Map)
import GHC.Generics
import System.FilePath
import System.Posix.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.InterfaceTypes

import Data.List.NonEmpty (NonEmpty)
--import qualified Data.List.NonEmpty as NonEmpty


-- | The kind of project being managed by a 'QueryEnv' (pun intended).
data ProjType
    = V1    -- ^ @cabal v1-build@ project, see 'DistDirV1'
    | V2    -- ^ @cabal v2-build@ project, see 'DistDirV2'
    | Stack -- ^ @stack@ project.

-- | The location of a project. The kind of location marker given determines the
-- 'ProjType'. The project type of a given directory can be determined by trying
-- to access a set of marker files. See below.
data ProjLoc (pt :: ProjType) where
    -- | A @cabal v1-build@ project directory can be identified by one file
    -- ending in @.cabal@ existing in the directory. More than one such files
    -- existing is a user error. Note: For this project type the concepts of
    -- project and package coincide.
    ProjLocCabalFile :: { plCabalFile :: FilePath } -> ProjLoc 'V1

    -- | A @cabal v2-build@ project\'s marker file is called
    -- @cabal.project@. This configuration file points to the packages that make
    -- up this project.
    ProjLocV2Dir     :: { plV2Dir :: FilePath } -> ProjLoc 'V2

    -- | A @stack@ project\'s marker file is called @stack.yaml@. This
    -- configuration file points to the packages that make up this project.
    ProjLocStackDir  :: { plStackDir :: FilePath } -> ProjLoc 'Stack

plV1Dir :: ProjLoc 'V1 -> FilePath
plV1Dir (ProjLocCabalFile cabal_file) = takeDirectory cabal_file

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

    -- | Build directory for stack, aka. /work-dir/. Optionally override Stack's
    -- /work-dir/. If you just want to use Stack's default set to @Nothing@
    DistDirStack :: Maybe RelativePath -> DistDir 'Stack

-- | Environment for running a 'Query' value. The real constructor is
-- not exposed, use the 'mkQueryEnv' smart constructor instead. The field
-- accessors are exported and may be used to override the defaults, see below.
type QueryEnv (pt :: ProjType)
    = QueryEnvI QueryCache pt

data QueryEnvI c (pt :: ProjType) = QueryEnv
    { qeReadProcess
          :: !(Maybe FilePath -> FilePath -> [String] -> String -> IO String)
    -- ^ Field accessor for 'QueryEnv'. Function used to to start
    -- processes. Useful if you need to, for example, redirect standard error
    -- output of programs started by cabal-helper.

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

    -- ^ Cache for query results, only accessible when type parameter @cache@ is
    -- instantiated and not forall quantified.
    , qeCacheRef     :: !(IORef (c pt))
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
data Unit pt = Unit
    { uUnitId      :: !UnitId
    , uPackageDir  :: !FilePath
    , uCabalFile   :: !CabalFile
    , uDistDir     :: !DistDirLib
    , uImpl        :: !(UnitImpl pt)
    }

data UnitImpl pt where
  UnitImplV1 :: UnitImpl 'V1

  UnitImplV2 ::
    { uiV2ComponentNames :: ![ChComponentName]
    , uiV2Components     :: ![String]
    } -> UnitImpl 'V2

  UnitImplStack :: UnitImpl 'Stack

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

-- | The information extracted from a 'Unit's on-disk configuration.
data UnitInfo = UnitInfo
    { uiUnitId                :: !UnitId
    -- ^ A unique identifier of this init within the project.

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
    } deriving (Eq, Ord, Read, Show)

-- | Files relevant to the project-scope configuration of a project. We gather
-- them here so we can refer to their paths conveniently.
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
    deriving (Eq)

data ProjInfo pt = ProjInfo
  { piCabalVersion     :: !Version
  , piProjConfModTimes :: !ProjConfModTimes
  , piUnits            :: !(NonEmpty (Unit pt))
  , piImpl             :: !(ProjInfoImpl pt)
  }

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

data UnitModTimes = UnitModTimes
    { umtPkgYaml     :: !(Maybe (FilePath, EpochTime))
    , umtCabalFile   :: !(FilePath, EpochTime)
    , umtSetupConfig :: !(Maybe (FilePath, EpochTime))
    } deriving (Eq, Ord, Read, Show)

newtype CabalFile = CabalFile FilePath

data StackProjPaths = StackProjPaths
    { sppGlobalPkgDb :: !PackageDbDir
    , sppSnapPkgDb   :: !PackageDbDir
    , sppLocalPkgDb  :: !PackageDbDir
    , sppCompExe     :: !FilePath
    }


-- Beware: GHC 8.0.2 doesn't like these being recursively defined for some
-- reason so just keep them unrolled.
type Verbose = (?verbose :: Bool)
type Env     = (?cprogs :: CompPrograms, ?progs :: Programs, ?verbose :: Bool)
type Progs   = (?cprogs :: CompPrograms, ?progs :: Programs)
type CProgs  = (?cprogs :: CompPrograms)

-- | Configurable paths to various programs we use.
data Programs = Programs {
      -- | The path to the @cabal@ program.
      cabalProgram  :: FilePath,

      -- | The path to the @stack@ program.
      stackProgram  :: FilePath
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

data CompPrograms = CompPrograms
    { ghcProgram    :: FilePath
    -- ^ The path to the @ghc@ program.

    , ghcPkgProgram :: FilePath
    -- ^ The path to the @ghc-pkg@ program. If not changed it will be derived
    -- from the path to 'ghcProgram'.
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

-- | By default all programs use their unqualified names, i.e. they will be
-- searched for on @PATH@.
defaultPrograms :: Programs
defaultPrograms = Programs "cabal" "stack"

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
newtype PackageEnvFile = PackageEnvFile { unPackageEnvFile :: FilePath }

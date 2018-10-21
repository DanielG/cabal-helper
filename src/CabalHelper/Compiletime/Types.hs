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
  StandaloneDeriving, GADTs, DataKinds, KindSignatures, ImplicitParams,
  ConstraintKinds, RankNTypes #-}

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
import System.Posix.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.InterfaceTypes


-- | The kind of project being managed by a 'QueryEnv' (pun intended).
data ProjType
    = V1    -- ^ @cabal v1-build@ project, see 'DistDirV1'
    | V2    -- ^ @cabal v2-build@ project, see 'DistDirV2'
    | Stack -- ^ @stack@ project.

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

    -- | A @stack@ project\'s marker file is called @stack.yaml@. This
    -- configuration file points to the packages that make up this project.
    ProjDirStack :: FilePath -> ProjDir 'Stack

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

    , uiPackageId             :: !(String, Version)
    -- ^ The package-name and version this unit belongs to.

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

  ProjInfoStack ::
    { piStackProjConfModTimes :: !(ProjConfModTimes 'Stack)
    , piStackUnits            :: ![Unit]
    , piStackProjPaths        :: !StackProjPaths
    } -> ProjInfo 'Stack

data ProjConfModTimes pt where
    ProjConfModTimesV1
        :: !(FilePath, EpochTime)     -> ProjConfModTimes 'V1
    ProjConfModTimesV2
        :: !([(FilePath, EpochTime)]) -> ProjConfModTimes 'V2
    ProjConfModTimesStack
        :: !(FilePath, EpochTime)     -> ProjConfModTimes 'Stack

deriving instance Eq (ProjConfModTimes pt)

data UnitModTimes = UnitModTimes
    { umtCabalFile   :: !(FilePath, EpochTime)
    , umtSetupConfig :: !(FilePath, EpochTime)
    } deriving (Eq, Ord, Read, Show)

newtype CabalFile = CabalFile FilePath

data StackProjPaths = StackProjPaths
    { sppGlobalPkgDb :: !PackageDbDir
    , sppSnapPkgDb   :: !PackageDbDir
    , sppLocalPkgDb  :: !PackageDbDir
    }

type Verbose = (?verbose :: Bool)
type Progs = (?progs :: Programs)
-- TODO: rname to `CompEnv` or something
type Env =
    ( ?verbose :: Bool
    , ?progs   :: Programs
    )

-- | Configurable paths to various programs we use.
data Programs = Programs {
      -- | The path to the @cabal@ program.
      cabalProgram  :: FilePath,

      -- | The path to the @stack@ program.
      stackProgram  :: FilePath,

      -- | The path to the @ghc@ program.
      ghcProgram    :: FilePath,

      -- | The path to the @ghc-pkg@ program. If
      -- not changed it will be derived from the path to 'ghcProgram'.
      ghcPkgProgram :: FilePath
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

-- | By default all programs use their unqualified names, i.e. they will be
-- searched for on @PATH@.
defaultPrograms :: Programs
defaultPrograms = Programs "cabal" "stack" "ghc" "ghc-pkg"


data CompileOptions = CompileOptions
    { oVerbose       :: Bool
    , oCabalPkgDb    :: Maybe PackageDbDir
    , oCabalVersion  :: Maybe Version
    , oPrograms      :: Programs
    }

oCabalProgram :: Env => FilePath
oCabalProgram = cabalProgram ?progs

oGhcProgram :: Env => FilePath
oGhcProgram = ghcProgram ?progs

oGhcPkgProgram :: Env => FilePath
oGhcPkgProgram = ghcPkgProgram ?progs

defaultCompileOptions :: CompileOptions
defaultCompileOptions =
    CompileOptions False Nothing Nothing defaultPrograms

newtype PackageDbDir = PackageDbDir { unPackageDbDir :: FilePath }

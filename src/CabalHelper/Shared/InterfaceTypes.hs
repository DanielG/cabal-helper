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

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DefaultSignatures #-}

{-|
Module      : CabalHelper.Shared.InterfaceTypes
Description : Types which are used by c-h library and executable to communicate
License     : GPL-3

These types are used to communicate between the cabal-helper library and helper
executable, using Show/Read. If any types in this module change the major
version must be bumped since this will be exposed in the @Distribution.Helper@
module.

The cached executables in @$XDG_CACHE_HOME/cabal-helper@ use the cabal-helper
version (among other things) as a cache key so we don't need to worry about
talking to an old executable.
-}
module CabalHelper.Shared.InterfaceTypes where

import GHC.Generics
import Data.Version
import Data.Map.Strict (Map)

data ChResponse
    = ChResponseComponentsInfo (Map ChComponentName ChComponentInfo)
    | ChResponseList           [String]
    | ChResponsePkgDbs         [ChPkgDb]
    | ChResponseLbi            String
    | ChResponseVersion        (String, Version)
    | ChResponseLicenses       [(String, [(String, Version)])]
    | ChResponseFlags          [(String, Bool)]
  deriving (Eq, Ord, Read, Show, Generic)

data ChComponentName = ChSetupHsName
                     | ChLibName ChLibraryName
                     | ChFLibName String
                     | ChExeName String
                     | ChTestName String
                     | ChBenchName String
  deriving (Eq, Ord, Read, Show, Generic)

data ChLibraryName = ChMainLibName
                   | ChSubLibName String
  deriving (Eq, Ord, Read, Show, Generic)

newtype ChModuleName = ChModuleName String
    deriving (Eq, Ord, Read, Show, Generic)

data ChComponentInfo = ChComponentInfo
    { ciComponentName         :: ChComponentName
    -- ^ The component\'s type and name

    , ciGhcOptions            :: [String]
    -- ^ Full set of GHC options, ready for loading this component into GHCi.

    , ciGhcSrcOptions         :: [String]
    -- ^ Only search path related GHC options.

    , ciGhcPkgOptions         :: [String]
    -- ^ Only package related GHC options, sufficient for things don't need to
    -- access any home modules.

    , ciGhcLangOptions        :: [String]
    -- ^ Only Haskell language extension related options, i.e. @-XSomeExtension@

    , ciSourceDirs            :: [String]
    -- ^ A component's @hs-source-dirs@ field, note that this only contains the
    -- directories specified by the cabal file, however cabal also adds the
    -- output directory of preprocessors to GHC's search path when
    -- building. TODO: make this easier to use.

    , ciEntrypoints           :: ChEntrypoint
    -- ^ Modules or files Cabal would have the compiler build directly. Can be
    -- used to compute the home module closure for a component.

    , ciNeedsBuildOutput      :: NeedsBuildOutput
    -- ^ If a component has a non-default module renaming (backpack) it cannot
    -- be built in memory and instead needs proper build output.
    } deriving (Eq, Ord, Read, Show)

-- TODO: we know the source-dir now so we can resolve ChSetupEntrypoint
-- internally
data ChEntrypoint = ChSetupEntrypoint -- ^ Almost like 'ChExeEntrypoint' but
                                      -- @main-is@ could either be @"Setup.hs"@
                                      -- or @"Setup.lhs"@. Since we don't know
                                      -- where the source directory is you have
                                      -- to find these files.
                  | ChLibEntrypoint { chExposedModules :: [ChModuleName]
                                    , chOtherModules   :: [ChModuleName]
                                    , chSignatures     :: [ChModuleName] -- backpack only
                                    }
                  | ChExeEntrypoint { chMainIs         :: FilePath
                                    , chOtherModules   :: [ChModuleName]
                                    } deriving (Eq, Ord, Read, Show, Generic)

data ChPkgDb = ChPkgGlobal
             | ChPkgUser
             | ChPkgSpecific FilePath
               deriving (Eq, Ord, Read, Show, Generic)

data NeedsBuildOutput = ProduceBuildOutput | NoBuildOutput
               deriving (Eq, Ord, Read, Show, Generic)

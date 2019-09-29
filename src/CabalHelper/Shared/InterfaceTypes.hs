-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DefaultSignatures #-}

{-|
Module      : CabalHelper.Shared.InterfaceTypes
Description : Types which are used by c-h library and executable to communicate
License     : Apache-2.0

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
    | ChResponseLbi            String
    | ChResponseVersion        (String, Version)
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

newtype ChModuleName = ChModuleName { unChModuleName :: String }
    deriving (Eq, Ord, Read, Show, Generic)

data ChComponentInfo = ChComponentInfo
    { ciComponentName         :: ChComponentName
    -- ^ The component\'s type and name

    , ciGhcOptions            :: [String]
    -- ^ Full set of GHC options, ready for loading this component into GHCi.

    , ciSourceDirs            :: [String]
    -- ^ A component's @hs-source-dirs@ field, note that this only contains the
    -- directories specified by the cabal file, however cabal also adds the
    -- output directory of preprocessors to GHC's search path when
    -- building. TODO: make this easier to use.

    , ciEntrypoints           :: ChEntrypoint
    -- ^ Modules or files Cabal would have the compiler build directly. Can be
    -- used to compute the home module closure for a component.
    } deriving (Eq, Ord, Read, Show)

data ChEntrypoint
    = ChSetupEntrypoint
      { chMainIs :: FilePath
      }
    | ChLibEntrypoint
      { chExposedModules :: [ChModuleName]
      , chOtherModules   :: [ChModuleName]
      , chSignatures     :: [ChModuleName] -- backpack only
      }
    | ChExeEntrypoint
      { chMainIs         :: FilePath
      , chOtherModules   :: [ChModuleName]
      } deriving (Eq, Ord, Read, Show, Generic)

data ChPkgDb = ChPkgGlobal
             | ChPkgUser
             | ChPkgSpecific FilePath
               deriving (Eq, Ord, Read, Show, Generic)

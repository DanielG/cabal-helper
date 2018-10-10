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

{-# LANGUAGE CPP, BangPatterns, RecordWildCards, RankNTypes, ViewPatterns,
  TupleSections #-}

#ifdef MIN_VERSION_Cabal
#undef CH_MIN_VERSION_Cabal
#define CH_MIN_VERSION_Cabal MIN_VERSION_Cabal
#endif

module CabalHelper.Runtime.Compat
    ( UnitId
    , componentNameToCh
    , unUnqualComponentName'
    , componentNameFromComponent
    , componentOutDir
    ) where

import System.FilePath

import Distribution.PackageDescription
  ( PackageDescription
  , GenericPackageDescription(..)
  , Flag(..)
  , FlagName
  , FlagAssignment
  , Executable(..)
  , Library(..)
  , TestSuite(..)
  , Benchmark(..)
  , BuildInfo(..)
  , TestSuiteInterface(..)
  , BenchmarkInterface(..)
  , withLib
  )
import Distribution.Simple.LocalBuildInfo
  ( ComponentName(..)
  , Component(..)
  , LocalBuildInfo(..)
  )


#if CH_MIN_VERSION_Cabal(1,24,0)
import Distribution.Package (UnitId)
#else
import Distribution.Package (InstalledPackageId)
#endif

#if CH_MIN_VERSION_Cabal(1,25,0)
-- >=1.25
import Distribution.PackageDescription
  ( unFlagName
  -- , mkFlagName
  )
import Distribution.Types.ForeignLib
  ( ForeignLib(..)
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , unUnqualComponentName
  )
#else
-- <1.25
import Distribution.PackageDescription
  ( FlagName(FlagName)
  )
#endif

#if CH_MIN_VERSION_Cabal(2,0,0)
-- CPP >= 2.0
import Distribution.Simple.LocalBuildInfo
  ( allLibModules
  , componentBuildDir
  )
import Distribution.Simple.Register
  ( internalPackageDBPath
  )
import Distribution.Backpack
  ( OpenUnitId(..),
    OpenModule(..)
  )
import Distribution.ModuleName
  ( ModuleName
  )
import Distribution.Types.ComponentId
  ( unComponentId
  )
import Distribution.Types.ComponentLocalBuildInfo
  ( maybeComponentInstantiatedWith
  )
import Distribution.Types.ModuleRenaming
  ( ModuleRenaming(..),
    isDefaultRenaming
  )
import Distribution.Types.MungedPackageId
  ( MungedPackageId
  )
import Distribution.Types.UnitId
  ( UnitId
  , unDefUnitId
  , unUnitId
  )
import Distribution.Types.UnitId
  ( DefUnitId
  )
import Distribution.Utils.NubList
  ( toNubListR
  )
import Distribution.Version
  ( versionNumbers
  , mkVersion
  )
import qualified Distribution.InstalledPackageInfo as Installed
#endif

#if CH_MIN_VERSION_Cabal(2,2,0)
import Distribution.Types.GenericPackageDescription
  ( unFlagAssignment
  )
#endif


import CabalHelper.Shared.Sandbox
import CabalHelper.Shared.Common
import CabalHelper.Shared.InterfaceTypes







#if !CH_MIN_VERSION_Cabal(1,24,0)
type UnitId = InstalledPackageId
#endif



componentNameToCh :: ComponentName -> ChComponentName
componentNameToCh CLibName = ChLibName
#if CH_MIN_VERSION_Cabal(2,0,0)
componentNameToCh (CSubLibName n) = ChSubLibName (unUnqualComponentName' n)
componentNameToCh (CFLibName n) = ChFLibName (unUnqualComponentName' n)
#endif
componentNameToCh (CExeName n) = ChExeName (unUnqualComponentName' n)
componentNameToCh (CTestName n) = ChTestName (unUnqualComponentName' n)
componentNameToCh (CBenchName n) = ChBenchName (unUnqualComponentName' n)


#if CH_MIN_VERSION_Cabal(1,25,0)
-- CPP >= 1.25
unUnqualComponentName' :: UnqualComponentName -> String
unUnqualComponentName' = unUnqualComponentName
#else
unUnqualComponentName' :: a -> a
unUnqualComponentName' = id
#endif


componentNameFromComponent :: Component -> ComponentName
#if !CH_MIN_VERSION_Cabal(1,25,0)
-- CPP < 1.25
componentNameFromComponent (CLib Library {}) = CLibName
#elif CH_MIN_VERSION_Cabal(1,25,0)
-- CPP >= 1.25 (redundant)
componentNameFromComponent (CLib Library { libName = Nothing }) = CLibName
componentNameFromComponent (CLib Library { libName = Just n })  = CSubLibName n
componentNameFromComponent (CFLib ForeignLib {..}) = CFLibName foreignLibName
#endif
componentNameFromComponent (CExe Executable {..}) = CExeName exeName
componentNameFromComponent (CTest TestSuite {..}) = CTestName testName
componentNameFromComponent (CBench Benchmark {..}) = CBenchName benchmarkName


componentOutDir :: LocalBuildInfo -> Component -> FilePath
componentOutDir lbi (CLib Library {..})=
    buildDir lbi
#if CH_MIN_VERSION_Cabal(2,0,0)
componentOutDir lbi (CFLib ForeignLib {..}) =
    componentOutDir' lbi (unUnqualComponentName foreignLibName)
#endif
componentOutDir lbi (CExe Executable {..}) =
    componentOutDir' lbi (unUnqualComponentName' exeName)
componentOutDir lbi (CTest TestSuite { testInterface = TestSuiteLibV09 _ _, ..}) =
    componentOutDir' lbi (unUnqualComponentName' testName ++ "Stub")
componentOutDir lbi (CTest TestSuite { testInterface = _, ..}) =
    componentOutDir' lbi (unUnqualComponentName' testName)
componentOutDir lbi (CBench Benchmark { benchmarkInterface = _, ..})=
    componentOutDir' lbi (unUnqualComponentName' benchmarkName)

componentOutDir' :: LocalBuildInfo -> String -> FilePath
componentOutDir' lbi compName' =
  ----- Copied from Distribution/Simple/GHC.hs:buildOrReplExe
  let targetDir = (buildDir lbi) </> compName'
      compDir    = targetDir </> (compName' ++ "-tmp")
  in compDir

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
  KindSignatures, ImplicitParams, ConstraintKinds #-}

{-|
Module      : CabalHelper.Compiletime.Types
Description : Types used throughout
License     : GPL-3
-}

module CabalHelper.Compiletime.Types where

import Data.Version
import Data.Typeable
import GHC.Generics

type Env = (?opts :: CompileOptions)

-- | Paths or names of various programs we need.
data Programs = Programs {
      -- | The path to the @cabal@ program.
      cabalProgram  :: FilePath,

      -- | The path to the @ghc@ program.
      ghcProgram    :: FilePath,

      -- | The path to the @ghc-pkg@ program. If
      -- not changed it will be derived from the path to 'ghcProgram'.
      ghcPkgProgram :: FilePath
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

-- | Default all programs to their unqualified names, i.e. they will be searched
-- for on @PATH@.
defaultPrograms :: Programs
defaultPrograms = Programs "cabal" "ghc" "ghc-pkg"

data CompileOptions = CompileOptions
    { oVerbose       :: Bool
    , oCabalPkgDb    :: Maybe PackageDbDir
    , oCabalVersion  :: Maybe Version
    , oPrograms      :: Programs
    }

oCabalProgram :: Env => FilePath
oCabalProgram = cabalProgram $ oPrograms ?opts

oGhcProgram :: Env => FilePath
oGhcProgram = ghcProgram $ oPrograms ?opts

oGhcPkgProgram :: Env => FilePath
oGhcPkgProgram = ghcPkgProgram $ oPrograms ?opts

defaultCompileOptions :: CompileOptions
defaultCompileOptions =
    CompileOptions False Nothing Nothing defaultPrograms

newtype PackageDbDir = PackageDbDir { unPackageDbDir :: FilePath }

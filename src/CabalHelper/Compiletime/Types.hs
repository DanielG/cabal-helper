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
import CabalHelper.Shared.InterfaceTypes

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

      -- | The path to the @ghc@ program.
      ghcProgram    :: FilePath,

      -- | The path to the @ghc-pkg@ program. If
      -- not changed it will be derived from the path to 'ghcProgram'.
      ghcPkgProgram :: FilePath
    } deriving (Eq, Ord, Show, Read, Generic, Typeable)

-- | By default all programs use their unqualified names, i.e. they will be
-- searched for on @PATH@.
defaultPrograms :: Programs
defaultPrograms = Programs "cabal" "ghc" "ghc-pkg"

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

-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DefaultSignatures #-}

{-|
Module      : CabalHelper.Compiletime.Types
Description : Types used throughout
License     : AGPL-3
-}

module CabalHelper.Compiletime.Types where

import Data.Version

data Options = Options {
          oVerbose       :: Bool
        , oGhcProgram    :: FilePath
        , oGhcPkgProgram :: FilePath
        , oCabalProgram  :: FilePath
        , oCabalVersion  :: Maybe Version
        , oCabalPkgDb    :: Maybe PackageDbDir
}

newtype PackageDbDir = PackageDbDir { unPackageDbDir :: FilePath }

defaultOptions :: Options
defaultOptions = Options False "ghc" "ghc-pkg" "cabal" Nothing Nothing

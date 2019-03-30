-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2019  Daniel Gröber <cabal-helper@dxld.at>
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

{-|
Module      : Distribution.Helper.Discover
Description : Finding project contexts
License     : GPL-3
Maintainer  : cabal-helper@dxld.at
Portability : portable
-}

-- TODO: $ sed -e s/DistDir/BuildDir/

module Distribution.Helper.Discover
  ( findProjects
  , findDistDirs
  , findDistDirsWithHints
  ) where

import CabalHelper.Compiletime.Types

findProjects :: FilePath -> IO [Ex ProjLoc]
findDistDirs :: ProjLoc pt -> [DistDir pt]
findDistDirsWithHints :: ProjLoc pt -> [FilePath] -> [DistDir pt]

findProjects = undefined
findDistDirs = undefined
findDistDirsWithHints = undefined

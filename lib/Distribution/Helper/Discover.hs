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

{-# LANGUAGE GADTs #-}

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

import Control.Monad.Writer
import Data.List
import System.Directory
import System.FilePath

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Cabal
import CabalHelper.Compiletime.Compat.Directory

findProjects :: FilePath -> IO [Ex ProjLoc]
findDistDirs :: ProjLoc pt -> [DistDir pt]
findDistDirsWithHints :: ProjLoc pt -> [FilePath] -> [DistDir pt]

findProjects dir = execWriterT $ do
  let cabalProject = dir </> "cabal.project"
  whenM (liftIO $ doesFileExist cabalProject) $
    tell [Ex $ ProjLocV2File cabalProject]
  let stackYaml = dir </> "stack.yaml"
  whenM (liftIO $ doesFileExist stackYaml) $
    tell [Ex $ ProjLocStackYaml stackYaml]
  join $ traverse (tell . pure . Ex . ProjLocV1Dir . takeDirectory) <$>
    liftIO (findCabalFiles dir)

findDistDirs (ProjLocV1CabalFile cabal _) =
  [DistDirCabal SCV1 $ replaceFileName cabal "dist/"]
findDistDirs (ProjLocV1Dir dir) = [DistDirCabal SCV1 $ dir </> "dist/"]
findDistDirs (ProjLocV2File cabal) =
  [DistDirCabal SCV2 $ replaceFileName cabal "dist-newstyle/"]
findDistDirs (ProjLocV2Dir dir) = [DistDirCabal SCV2 $ dir </> "dist-newstyle/"]
findDistDirs (ProjLocStackYaml _) = [DistDirStack Nothing]

findDistDirsWithHints = undefined

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles dir = do
  fs <- listDirectory dir
  let cs = filter (".cabal" `isSuffixOf`) fs
  filterM doesFileExist cs

whenM :: Monad m => m Bool -> m () -> m ()
whenM p x = p >>= (`when` x)

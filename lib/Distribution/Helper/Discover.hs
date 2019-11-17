-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2019  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}

{-|
Module      : Distribution.Helper.Discover
Description : Finding project contexts
License     : Apache-2.0
Maintainer  : cabal-helper@dxld.at
Portability : portable
-}

-- TODO: $ sed -e s/DistDir/BuildDir/

module Distribution.Helper.Discover
  ( findProjects
  , getDefaultDistDir
  , isValidDistDir
  ) where

import Control.Monad.Writer
import Data.Version
import System.Directory
import System.FilePath

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Cabal

-- | @findProjects dir@. Find available project instances in @dir@.
--
-- For example, if the given directory contains both a @cabal.project@ and
-- a @stack.yaml@ file:
--
-- >>> findProjects "."
-- [ Ex (ProjLocStackYaml "./stack.yaml"), Ex (ProjLocCabalV2File "./cabal.project") ]
--
-- Note that this function only looks for "default" project markers. If you
-- want to for example support the common pattern of having multiple
-- @stack-<GHC_VER>.yaml@ files simply fill out a 'ProjLoc' yourself. In
-- this case 'ProjLocStackYaml'.
findProjects :: FilePath -> IO [Ex ProjLoc]
findProjects dir = execWriterT $ do
  let cabalProject = dir </> "cabal.project"
  whenM (liftIO $ doesFileExist cabalProject) $
    tell [Ex $ ProjLocV2File cabalProject dir]
  let stackYaml = dir </> "stack.yaml"
  whenM (liftIO $ doesFileExist stackYaml) $
    tell [Ex $ ProjLocStackYaml stackYaml]

  join $ traverse (tell . pure . Ex . ProjLocV2Dir . takeDirectory) <$>
    liftIO (findCabalFile dir)

  join $ traverse (tell . pure . Ex . ProjLocV1Dir . takeDirectory) <$>
    liftIO (findCabalFile dir)


-- | @getDefaultDistDir pl@. Get the default dist-dir for the given project.
--
-- Note that the path in the returned dist-dir might not exist yet if the
-- build-tool has never been run for this project before. This is fine as
-- far as @cabal-helper@ is concerned. It will simply invoke the build-tool
-- as needed to answer the requested queries.
getDefaultDistDir :: ProjLoc pt -> DistDir pt
getDefaultDistDir (ProjLocV1CabalFile _cabal_file pkgdir) =
  DistDirCabal SCV1 $ pkgdir </> "dist"
getDefaultDistDir (ProjLocV1Dir pkgdir) =
  DistDirCabal SCV1 $ pkgdir </> "dist"
getDefaultDistDir (ProjLocV2File _cabal_project projdir) =
  DistDirCabal SCV2 $ projdir </> "dist-newstyle"
getDefaultDistDir (ProjLocV2Dir projdir) =
  DistDirCabal SCV2 $ projdir </> "dist-newstyle"
getDefaultDistDir (ProjLocStackYaml _) =
  DistDirStack Nothing

-- | @isValidDistDir distdir@. Check if @distdir@ looks like a valid
-- build-dir for it's project type. We just check if characteristic marker
-- files for the associated project type exist.
--
-- If the project type does not have a way to do this (for example
-- 'DistDirStack') check we return 'Nothing'.
isValidDistDir :: DistDir pt -> IO (Maybe Bool)
isValidDistDir (DistDirCabal cpt dir) = do
  fmap Just $ doesFileExist $ dir </> cabalProjTypeMarkerFile cpt
isValidDistDir DistDirStack{} =
  return Nothing

cabalProjTypeMarkerFile :: SCabalProjType pt -> FilePath
cabalProjTypeMarkerFile SCV1 = "setup-config"
cabalProjTypeMarkerFile SCV2 = "cache" </> "plan.json"

whenM :: Monad m => m Bool -> m () -> m ()
whenM p x = p >>= (`when` x)

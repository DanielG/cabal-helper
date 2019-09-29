-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2017  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-|
Module      : CabalHelper.Shared.Sandbox
Description : Extracting information from @cabal.sandbox.config@ files
License     : Apache-2.0
-}

module CabalHelper.Compiletime.Sandbox where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List
import System.FilePath
import Prelude

import qualified Data.Traversable as T

import CabalHelper.Shared.Common
import CabalHelper.Compiletime.Program.GHC
    ( GhcVersion (..), showGhcVersion )

-- | Get the path to the sandbox package-db in a project
getSandboxPkgDb :: String
             -- ^ Cabal build platform, i.e. @buildPlatform@
             -> GhcVersion
             -- ^ GHC version (@cProjectVersion@ is your friend)
             -> FilePath
             -- ^ Path to the cabal package root directory (containing the
             -- @cabal.sandbox.config@ file)
             -> IO (Maybe FilePath)
getSandboxPkgDb platform ghcVer projdir = do
  mConf <-
      T.traverse readFile =<< mightExist (projdir </> "cabal.sandbox.config")
  return $ fixPkgDbVer <$> (extractSandboxDbDir =<< mConf)

 where
   fixPkgDbVer dir =
       case takeFileName dir == ghcSandboxPkgDbDir platform ghcVer of
         True -> dir
         False -> takeDirectory dir </> ghcSandboxPkgDbDir platform ghcVer

ghcSandboxPkgDbDir :: String -> GhcVersion -> String
ghcSandboxPkgDbDir platform ghcVer =
   platform ++ "-ghc-" ++ showGhcVersion ghcVer ++ "-packages.conf.d"

-- | Extract the sandbox package db directory from the cabal.sandbox.config
-- file. Exception is thrown if the sandbox config file is broken.
extractSandboxDbDir :: String -> Maybe FilePath
extractSandboxDbDir conf = extractValue <$> parse conf
  where
    key = "package-db:"
    keyLen = length key

    parse = listToMaybe . filter (key `isPrefixOf`) . lines
    extractValue = CabalHelper.Compiletime.Sandbox.dropWhileEnd isSpace . dropWhile isSpace . drop keyLen

-- dropWhileEnd is not provided prior to base 4.5.0.0.
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

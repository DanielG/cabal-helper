-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2017  Daniel Gröber <cabal-helper@dxld.at>
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
Module      : CabalHelper.Shared.Sandbox
Description : Extracting information from @cabal.sandbox.config@ files
License     : GPL-3
-}

module CabalHelper.Compiletime.Sandbox where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.List
import Data.Version
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

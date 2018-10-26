-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2018  Daniel Gröber <cabal-helper@dxld.at>
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
Module      : CabalHelper.Compiletime.Program.GHC
Description : GHC program interface
License     : GPL-3
-}

module CabalHelper.Compiletime.Program.GHC where

import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.String
import Data.Maybe
import Data.Version
import System.Exit
import System.FilePath
import System.Directory

import CabalHelper.Shared.Common
  (parseVer, trim, appCacheDir, parsePkgId)
import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Cabal
  (CabalVersion(..), showCabalVersion)
import CabalHelper.Compiletime.Process
import CabalHelper.Compiletime.Log

data GhcInvocation = GhcInvocation
    { giOutDir          :: FilePath
    , giOutput          :: FilePath
    , giCPPOptions      :: [String]
    , giPackageDBs      :: [PackageDbDir]
    , giIncludeDirs     :: [FilePath]
    , giHideAllPackages :: Bool
    , giPackages        :: [String]
    , giWarningFlags    :: [String]
    , giInputs          :: [String]
    }

ghcVersion :: (Verbose, CProgs) => IO Version
ghcVersion =
  parseVer . trim <$> readProcess' (ghcProgram ?cprogs) ["--numeric-version"] ""

ghcPkgVersion :: (Verbose, CProgs) => IO Version
ghcPkgVersion =
  parseVer . trim . dropWhile (not . isDigit)
    <$> readProcess' (ghcPkgProgram ?cprogs) ["--version"] ""

createPkgDb :: (Verbose, CProgs) => CabalVersion -> IO PackageDbDir
createPkgDb cabalVer = do
  db@(PackageDbDir db_path) <- getPrivateCabalPkgDb cabalVer
  exists <- doesDirectoryExist db_path
  when (not exists) $
       callProcessStderr Nothing (ghcPkgProgram ?cprogs) ["init", db_path]
  return db

getPrivateCabalPkgDb :: (Verbose, CProgs) => CabalVersion -> IO PackageDbDir
getPrivateCabalPkgDb cabalVer = do
  appdir <- appCacheDir
  ghcVer <- ghcVersion
  let db_path =
        appdir </> "ghc-" ++ showVersion ghcVer ++ ".package-db"
               </> "Cabal-" ++ showCabalVersion cabalVer
  return $ PackageDbDir db_path

listCabalVersions
    :: (Verbose, Progs) => Maybe PackageDbDir -> MaybeT IO [Version]
listCabalVersions mdb = do
  let mdb_path = unPackageDbDir <$> mdb
  exists <- fromMaybe True <$>
    traverse (liftIO . doesDirectoryExist) mdb_path
  case exists of
    True -> MaybeT $ logIOError "listCabalVersions" $ Just <$> do
      let mdbopt = ("--package-conf="++) <$> mdb_path
          args = ["list", "--simple-output", "Cabal"] ++ maybeToList mdbopt
      catMaybes . map (fmap snd . parsePkgId . fromString) . words
               <$> readProcess' (ghcPkgProgram ?cprogs) args ""
    _ -> mzero

cabalVersionExistsInPkgDb
    :: (Verbose, Progs) => Version -> PackageDbDir -> IO Bool
cabalVersionExistsInPkgDb cabalVer db@(PackageDbDir db_path) = do
  exists <- doesDirectoryExist db_path
  case exists of
    False -> return False
    True -> fromMaybe False <$> runMaybeT (do
      vers <- listCabalVersions (Just db)
      return $ cabalVer `elem` vers)

invokeGhc :: Env => GhcInvocation -> IO (Either ExitCode FilePath)
invokeGhc GhcInvocation {..} = do
    rv <- callProcessStderr' Nothing (ghcProgram ?cprogs) $ concat
      [ [ "-outputdir", giOutDir
        , "-o", giOutput
        ]
      , map ("-optP"++) giCPPOptions
      , map ("-package-conf="++) $ unPackageDbDir <$> giPackageDBs
      , map ("-i"++) $ nub $ "" : giIncludeDirs
      , if giHideAllPackages then ["-hide-all-packages"] else []
      , concatMap (\p -> ["-package", p]) giPackages
      , giWarningFlags
      , ["--make"]
      , giInputs
      ]
    return $
      case rv of
        ExitSuccess -> Right giOutput
        e@(ExitFailure _) -> Left e

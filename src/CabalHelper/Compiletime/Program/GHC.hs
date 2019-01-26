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
  ( ResolvedCabalVersion, showResolvedCabalVersion, UnpackedCabalVersion
  , unpackedToResolvedCabalVersion, CabalVersion'(..) )
import CabalHelper.Compiletime.Process
import CabalHelper.Compiletime.Log

data GhcPackageSource
    = GPSAmbient
    | GPSPackageDBs ![PackageDbDir]
    | GPSPackageEnv !PackageEnvFile

data GhcInvocation = GhcInvocation
    { giOutDir          :: !FilePath
    , giOutput          :: !FilePath
    , giCPPOptions      :: ![String]
    , giPackageSource   :: !GhcPackageSource
    , giIncludeDirs     :: ![FilePath]
    , giHideAllPackages :: !Bool
    , giPackages        :: ![String]
    , giWarningFlags    :: ![String]
    , giInputs          :: ![String]
    }

newtype GhcVersion = GhcVersion { unGhcVersion :: Version }
    deriving (Eq, Ord, Read, Show)

showGhcVersion :: GhcVersion -> String
showGhcVersion (GhcVersion v) = showVersion v

ghcVersion :: (Verbose, CProgs) => IO GhcVersion
ghcVersion = GhcVersion .
  parseVer . trim <$> readProcess' (ghcProgram ?cprogs) ["--numeric-version"] ""

ghcPkgVersion :: (Verbose, CProgs) => IO Version
ghcPkgVersion =
  parseVer . trim . dropWhile (not . isDigit)
    <$> readProcess' (ghcPkgProgram ?cprogs) ["--version"] ""

createPkgDb :: (Verbose, CProgs) => UnpackedCabalVersion -> IO PackageDbDir
createPkgDb cabalVer = do
  db@(PackageDbDir db_path)
    <- getPrivateCabalPkgDb $ unpackedToResolvedCabalVersion cabalVer
  exists <- doesDirectoryExist db_path
  when (not exists) $
       callProcessStderr Nothing (ghcPkgProgram ?cprogs) ["init", db_path]
  return db

getPrivateCabalPkgDb :: (Verbose, CProgs) => ResolvedCabalVersion -> IO PackageDbDir
getPrivateCabalPkgDb cabalVer = do
  appdir <- appCacheDir
  ghcVer <- ghcVersion
  let db_path =
        appdir </> "ghc-" ++ showGhcVersion ghcVer ++ ".package-dbs"
               </> "Cabal-" ++ showResolvedCabalVersion cabalVer
  return $ PackageDbDir db_path

getPrivateCabalPkgEnv
    :: Verbose => GhcVersion -> Version -> IO PackageEnvFile
getPrivateCabalPkgEnv ghcVer cabalVer = do
  appdir <- appCacheDir
  let env_path =
        appdir </> "ghc-" ++ showGhcVersion ghcVer ++ ".package-envs"
               </> "Cabal-" ++ showVersion cabalVer ++ ".package-env"
  return $ PackageEnvFile env_path

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
    :: (Verbose, Progs) => CabalVersion' a -> PackageDbDir -> IO Bool
cabalVersionExistsInPkgDb cabalVer db@(PackageDbDir db_path) = do
  fromMaybe False <$> runMaybeT (do
    vers <- listCabalVersions (Just db)
    return $
      case (cabalVer, vers) of
        (CabalVersion ver, _) -> ver `elem` vers
        (CabalHEAD _, [_headver]) -> True
        (CabalHEAD _, _) ->
          error $ msg ++ db_path)
  where
    msg = "\
\Multiple Cabal versions in a HEAD package-db!\n\
\This shouldn't happen. However you can manually delete the following\n\
\directory to resolve this:\n    "

invokeGhc :: Env => GhcInvocation -> IO (Either ExitCode FilePath)
invokeGhc GhcInvocation {..} = do
    rv <- callProcessStderr' Nothing (ghcProgram ?cprogs) $ concat
      [ [ "-outputdir", giOutDir
        , "-o", giOutput
        ]
      , map ("-optP"++) giCPPOptions
      , if giHideAllPackages then ["-hide-all-packages"] else []
      , let packageFlags = concatMap (\p -> ["-package", p]) giPackages in
        case giPackageSource of
          GPSAmbient -> packageFlags
          GPSPackageDBs dbs -> concat
            [ map ("-package-conf="++) $ unPackageDbDir <$> dbs
            , packageFlags
            ]
          GPSPackageEnv env -> [ "-package-env=" ++ unPackageEnvFile env ]
      , map ("-i"++) $ nub $ "" : giIncludeDirs
      , giWarningFlags
      , ["--make"]
      , giInputs
      ]
    return $
      case rv of
        ExitSuccess -> Right giOutput
        e@(ExitFailure _) -> Left e

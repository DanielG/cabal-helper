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
Module      : CabalHelper.Compiletime.Program.Stack
Description : Stack program interface
License     : GPL-3
-}

{-# LANGUAGE GADTs, DataKinds #-}

module CabalHelper.Compiletime.Program.Stack where

import Control.Monad
import Data.Char
import Data.List hiding (filter)
import Data.String
import Data.Maybe
import Data.Function
import System.FilePath hiding ((<.>))
import Prelude

import qualified CabalHelper.Compiletime.Compat.ProgramDb as ProgDb
    ( defaultProgramDb, programPath, lookupProgram, ghcProgram, ghcPkgProgram)
import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Types.RelativePath

import Distribution.Simple.GHC as GHC (configure)
import Distribution.Verbosity (silent, deafening)

getUnit :: QueryEnvI c 'Stack -> CabalFile -> IO (Unit 'Stack)
getUnit qe cabal_file@(CabalFile cabal_file_path) = do
  let pkgdir = takeDirectory cabal_file_path
  let pkg_name = dropExtension $ takeFileName cabal_file_path
  look <- paths qe pkgdir
  let distdirv1 = look "dist-dir:"
  return $ Unit
    { uUnitId     = UnitId pkg_name
    , uPackageDir = pkgdir
    , uCabalFile  = cabal_file
    , uDistDir    = DistDirLib distdirv1
    , uImpl       = UnitImplStack
    }

-- TODO: patch ghc/ghc-pkg program paths like in ghc-mod when using stack so
-- compilation logic works even if no system compiler is installed

packageDistDir :: QueryEnvI c 'Stack -> FilePath -> IO FilePath
packageDistDir qe pkgdir = do
  look <- paths qe pkgdir
  return $ look "dist-dir:"

projPaths :: QueryEnvI c 'Stack -> IO StackProjPaths
projPaths qe@QueryEnv {qeProjLoc=ProjLocStackDir projdir} = do
  look <- paths qe projdir
  return StackProjPaths
    { sppGlobalPkgDb = PackageDbDir $ look "global-pkg-db:"
    , sppSnapPkgDb   = PackageDbDir $ look "snapshot-pkg-db:"
    , sppLocalPkgDb  = PackageDbDir $ look "local-pkg-db:"
    , sppCompExe     = look "compiler-exe:"
    }

paths :: QueryEnvI c 'Stack
      -> FilePath
      -> IO (String -> FilePath)
paths qe dir = do
    out <- qeReadProcess qe (Just dir) (stackProgram $ qePrograms qe)
      (workdirArg qe ++ [ "path" ]) ""
    return $ \k -> let Just x = lookup k $ map split $ lines out in x
  where
    split l = let (key, ' ' : val) = span (not . isSpace) l in (key, val)

listPackageCabalFiles :: QueryEnvI c 'Stack -> IO [CabalFile]
listPackageCabalFiles qe@QueryEnv{qeProjLoc=ProjLocStackDir projdir} = do
  out <- qeReadProcess qe (Just projdir) (stackProgram $ qePrograms qe)
    [ "ide", "packages", "--cabal-files" ] ""
  return $ map CabalFile $ lines out

workdirArg :: QueryEnvI c 'Stack -> [String]
workdirArg QueryEnv{qeDistDir=DistDirStack mworkdir} =
  maybeToList $ ("--work-dir="++) . unRelativePath <$> mworkdir


patchCompPrograms :: Verbose => StackProjPaths -> CompPrograms -> IO CompPrograms
patchCompPrograms StackProjPaths{sppCompExe} compProgs =
  guessCompProgramPaths $ patchGhc compProgs
  where 
    patchGhc cprogs = cprogs { ghcProgram = sppCompExe }

    -- | Determine ghc-pkg path from ghc path
    guessCompProgramPaths :: Verbose => CompPrograms -> IO CompPrograms
    guessCompProgramPaths progs = do
        let v | ?verbose  = deafening
              | otherwise = silent
            mGhcPath0    | same ghcProgram progs dprogs = Nothing
                        | otherwise = Just $ ghcProgram progs
            mGhcPkgPath0 | same ghcPkgProgram progs dprogs = Nothing
                        | otherwise = Just $ ghcPkgProgram progs
        (_compiler, _mplatform, progdb)
            <- GHC.configure
                  v
                  mGhcPath0
                  mGhcPkgPath0
                  ProgDb.defaultProgramDb
        let getProg p = ProgDb.programPath <$> ProgDb.lookupProgram p progdb
            mghcPath1    = getProg ProgDb.ghcProgram
            mghcPkgPath1 = getProg ProgDb.ghcPkgProgram
        return progs
          { ghcProgram    = fromMaybe (ghcProgram progs) mghcPath1
          , ghcPkgProgram = fromMaybe (ghcProgram progs) mghcPkgPath1
          }
      where
        same f o o'  = f o == f o'
        dprogs = defaultCompPrograms
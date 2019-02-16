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

import qualified CabalHelper.Compiletime.Cabal as Cabal
import Control.Exception (handle, throwIO)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.List hiding (filter)
import Data.String
import Data.Maybe
import Data.Function
import Data.Version
import System.Directory (findExecutable)
import System.FilePath hiding ((<.>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Text.Show.Pretty
import Prelude

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.Common

getUnit :: QueryEnvI c 'Stack -> CabalFile -> IO (Unit 'Stack)
getUnit
  qe@QueryEnv{qeProjLoc=ProjLocStackYaml stack_yaml}
  cabal_file@(CabalFile cabal_file_path)
  = do
  let projdir = takeDirectory stack_yaml
  let pkgdir = takeDirectory cabal_file_path
  let pkg_name = dropExtension $ takeFileName cabal_file_path
  look <- paths qe pkgdir
  let distdirv1_rel = look "dist-dir:"
  return $ Unit
    { uUnitId     = UnitId pkg_name
    , uPackageDir = pkgdir
    , uCabalFile  = cabal_file
    , uDistDir    = DistDirLib $ pkgdir </> distdirv1_rel
    , uImpl       = UnitImplStack
    }

projPaths :: QueryEnvI c 'Stack -> IO StackProjPaths
projPaths qe@QueryEnv {qeProjLoc=ProjLocStackYaml stack_yaml} = do
  look <- paths qe $ takeDirectory stack_yaml
  return StackProjPaths
    { sppGlobalPkgDb = PackageDbDir $ look "global-pkg-db:"
    , sppSnapPkgDb   = PackageDbDir $ look "snapshot-pkg-db:"
    , sppLocalPkgDb  = PackageDbDir $ look "local-pkg-db:"
    , sppCompExe     = look "compiler-exe:"
    }

paths :: QueryEnvI c 'Stack -> FilePath -> IO (String -> FilePath)
paths qe@QueryEnv{qeProjLoc=ProjLocStackYaml stack_yaml} cwd
  = do
  out <- readStackCmd qe (Just cwd) $
    workdirArg qe ++ [ "path", "--stack-yaml="++stack_yaml ]
  return $ \k -> let Just x = lookup k $ map split $ lines out in x
  where
    split l = let (key, ' ' : val) = span (not . isSpace) l in (key, val)

listPackageCabalFiles' :: QueryEnvI c 'Stack -> IO [CabalFile]
listPackageCabalFiles' qe@QueryEnv{qeProjLoc=ProjLocStackYaml stack_yaml}
  = handle ioerror $ do
  let projdir = takeDirectory stack_yaml
  out <- readStackCmd qe (Just projdir)
    [ "ide", "packages", "--cabal-files", "--stdout" ]
  return $ map CabalFile $ lines out
  where
    ioerror :: IOError -> IO a
    ioerror ioe = (=<<) (fromMaybe (throwIO ioe)) $ runMaybeT $ do
      stack_exe <- MaybeT $ findExecutable $ stackProgram $ qePrograms qe
      stack_ver_str
        <- liftIO $ trim <$> readStackCmd qe Nothing ["--numeric-version"]
      stack_ver <- MaybeT $ return $ parseVerMay stack_ver_str
      guard $ stack_ver < makeVersion [1,9,4]

      let prog_cfg = ppShow $ qePrograms qe

      liftIO $ hPutStrLn stderr $ printf
        "\nerror: stack version too old!\
        \\n\n\
        \You have '%s' installed but cabal-helper needs at least\n\
        \stack version 1.9.4+.\n\
        \\n\
        \FYI cabal-helper is using the following `stack` executable:\n\
        \  %s\n\
        \\n\
        \Additional debugging info: QueryEnv qePrograms =\n\
        \  %s\n" stack_ver_str stack_exe prog_cfg
      mzero

-- | Workaround for stack before 1.9.4
listPackageCabalFiles :: QueryEnvI c 'Stack -> IO [CabalFile]
listPackageCabalFiles qe@QueryEnv{qeProjLoc=ProjLocStackYaml stack_yaml}
  = handle ioerror $ do
  let projdir = takeDirectory stack_yaml
  out <- readStackCmd qe (Just projdir)
    -- [ "ide", "packages", "--cabal-files", "--stdout" ]
    [ "query", "locals" ]
  let packageDirs = catMaybes $ map getPath $ lines out
  cabalFiles <- mapM Cabal.findCabalFile $ filter (/= "") $ lines $ concat packageDirs
  return $ map CabalFile cabalFiles
  where
    ioerror :: IOError -> IO a
    ioerror ioe = (=<<) (fromMaybe (throwIO ioe)) $ runMaybeT $ do
      stack_exe <- MaybeT $ findExecutable $ stackProgram $ qePrograms qe
      stack_ver_str
        <- liftIO $ trim <$> readStackCmd qe Nothing ["--numeric-version"]
      stack_ver <- MaybeT $ return $ parseVerMay stack_ver_str
      guard $ stack_ver < makeVersion [1,9,3]

      let prog_cfg = ppShow $ qePrograms qe

      liftIO $ hPutStrLn stderr $ printf
        "\nerror: stack version too old!\
        \\n\n\
        \You have '%s' installed but cabal-helper needs at least\n\
        \stack version 1.9.4+.\n\
        \\n\
        \FYI cabal-helper is using the following `stack` executable:\n\
        \  %s\n\
        \\n\
        \Additional debugging info: QueryEnv qePrograms =\n\
        \  %s\n" stack_ver_str stack_exe prog_cfg
      mzero

getPath :: String -> Maybe String
getPath str = r
  where
    str' = dropWhile (==' ') str
    r = if isPrefixOf "path: " str'
    then Just (drop (length "path: ") str')
    else Nothing




workdirArg :: QueryEnvI c 'Stack -> [String]
workdirArg QueryEnv{qeDistDir=DistDirStack mworkdir} =
  maybeToList $ ("--work-dir="++) . unRelativePath <$> mworkdir

doStackCmd :: (QueryEnvI c 'Stack -> CallProcessWithCwd a)
           -> QueryEnvI c 'Stack -> Maybe FilePath -> [String] -> IO a
doStackCmd procfn qe mcwd args =
  let Programs{..} = qePrograms qe in
  procfn qe mcwd stackProgram $ stackArgsBefore ++ args ++ stackArgsAfter

readStackCmd :: QueryEnvI c 'Stack -> Maybe FilePath -> [String] -> IO String
callStackCmd :: QueryEnvI c 'Stack -> Maybe FilePath -> [String] -> IO ()

readStackCmd = doStackCmd (\qe -> qeReadProcess qe "")
callStackCmd = doStackCmd qeCallProcess

patchCompPrograms :: StackProjPaths -> CompPrograms -> CompPrograms
patchCompPrograms StackProjPaths{sppCompExe} cprogs =
  cprogs { ghcProgram = sppCompExe }

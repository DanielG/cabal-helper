-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-|
Module      : CabalHelper.Compiletime.Program.Stack
Description : Stack program interface
License     : Apache-2.0
-}

{-# LANGUAGE GADTs, DataKinds #-}

module CabalHelper.Compiletime.Program.Stack where

import Control.Exception (handle, throwIO)
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.List hiding (filter)
import Data.List.NonEmpty (NonEmpty(..))
import Data.String
import Data.Maybe
import Data.Function
import Data.Version
import System.Directory (findExecutable)
import System.FilePath hiding ((<.>))
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)
import Prelude

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Types.RelativePath
import CabalHelper.Shared.Common

getPackage :: QueryEnvI c 'Stack -> CabalFile -> IO (Package 'Stack)
getPackage qe cabal_file@(CabalFile cabal_file_path) = do
  let pkgdir = takeDirectory cabal_file_path
  -- this is kind of a hack but works even for unicode package names and
  -- besides stack even enforces this naming convention unlike cabal. This
  -- is the error you get if the names don't match:
  --
  -- cabal file path foo-bla.cabal does not match the package name it defines.
  -- Please rename the file to: foo.cabal
  -- For more information, see:
  --  https://github.com/commercialhaskell/stack/issues/317
  let pkg_name = dropExtension $ takeFileName cabal_file_path
  look <- paths qe pkgdir
  let distdirv1_rel = look "dist-dir:"
  let pkg = Package
        { pPackageName = pkg_name
        , pSourceDir = pkgdir
        , pCabalFile = cabal_file
        , pFlags = []
        , pUnits = (:|[]) $ Unit
          { uUnitId     = UnitId pkg_name
          , uDistDir    = DistDirLib $ pkgdir </> distdirv1_rel
          , uPackage    = pkg { pUnits = () }
          , uImpl       = UnitImplStack
          }
        }
  return pkg

projPaths :: QueryEnvI c 'Stack -> IO StackProjPaths
projPaths qe@QueryEnv {qeProjLoc} = do
  look <- paths qe $ plStackProjectDir qeProjLoc
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

listPackageCabalFiles :: QueryEnvI c 'Stack -> IO [CabalFile]
listPackageCabalFiles qe@QueryEnv{qeProjLoc}
  = handle ioerror $ do
  let projdir = plStackProjectDir qeProjLoc
  out <- readStackCmd qe (Just projdir)
    [ "ide", "packages", "--cabal-files", "--stdout" ]
  return $ map CabalFile $ lines out
  where
    ioerror :: IOError -> IO a
    ioerror ioe = (fromMaybe (throwIO ioe) =<<) $ runMaybeT $ do
      stack_exe <- MaybeT $ findExecutable $ stackProgram $ qePrograms qe
      stack_ver_str
        <- liftIO $ trim <$> readStackCmd qe Nothing ["--numeric-version"]
      stack_ver <- MaybeT $ return $ parseVerMay stack_ver_str
      guard $ stack_ver < makeVersion [1,9,4]

      let prog_cfg = show $ qePrograms qe

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

workdirArg :: QueryEnvI c 'Stack -> [String]
workdirArg QueryEnv{qeDistDir=DistDirStack mworkdir} =
  maybeToList $ ("--work-dir="++) . unRelativePath <$> mworkdir

doStackCmd :: (QueryEnvI c 'Stack -> CallProcessWithCwdAndEnv a)
           -> QueryEnvI c 'Stack
           -> Maybe FilePath -> [String] -> IO a
doStackCmd procfn qe mcwd args =
  let Programs{..} = qePrograms qe in
  procfn qe mcwd stackEnv stackProgram $
    stackProjArgs ++ args ++ stackUnitArgs

readStackCmd :: QueryEnvI c 'Stack -> Maybe FilePath -> [String] -> IO String
callStackCmd :: QueryEnvI c 'Stack -> Maybe FilePath -> [String] -> IO ()

readStackCmd = doStackCmd (\qe -> qeReadProcess qe "")
callStackCmd = doStackCmd qeCallProcess

patchCompPrograms :: StackProjPaths -> Programs -> Programs
patchCompPrograms StackProjPaths{sppCompExe} progs =
  progs { ghcProgram = sppCompExe }

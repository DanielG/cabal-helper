{-# LANGUAGE DataKinds, MultiWayIf, TupleSections, GADTs, OverloadedStrings #-}

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
Module      : CabalHelper.Compiletime.Program.Cabal
Description : cabal-install program interface
License     : Apache-2.0
-}

module CabalHelper.Compiletime.Program.CabalInstall where

import Control.Arrow ((&&&))
import qualified Cabal.Plan as CP
import Control.Monad
import Data.Coerce
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup ((<>))
import Data.Maybe
import Data.Version
import System.IO
import System.IO.Temp
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf
import Text.Read

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

import qualified CabalHelper.Compiletime.Cabal as Cabal
import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Program.GHC
  ( GhcVersion(..), createPkgDb )
import CabalHelper.Compiletime.Cabal
  ( CabalSourceDir(..), UnpackedCabalVersion, CabalVersion'(..), unpackCabalV1 )
import CabalHelper.Compiletime.Process
import CabalHelper.Shared.InterfaceTypes
  ( ChComponentName(..), ChLibraryName(..) )
import CabalHelper.Shared.Common
  ( parseVer, trim, appCacheDir, panicIO )

newtype CabalInstallVersion = CabalInstallVersion { cabalInstallVer :: Version }

data HEAD = HEAD deriving (Eq, Show)

cabalInstallVersion :: (Verbose, Progs) => IO CabalInstallVersion
cabalInstallVersion = do
  CabalInstallVersion . parseVer . trim
    <$> readProcess' (cabalProgram ?progs) ["--numeric-version"] ""

installCabalLibV1 :: Env => GhcVersion -> UnpackedCabalVersion -> IO PackageDbDir
installCabalLibV1 ghcVer cabalVer = do
  withSystemTempDirectory "cabal-helper.install-cabal-tmp" $ \tmpdir -> do
    installingMessage cabalVer
    srcdir <- unpackCabalV1 cabalVer tmpdir

    db <- createPkgDb cabalVer

    callCabalInstall db srcdir ghcVer cabalVer

    return db

installingMessage :: CabalVersion' a -> IO ()
installingMessage = message
  where
    message (CabalHEAD {}) = return () -- only used for tests
    message (CabalVersion ver) = do
      appdir <- appCacheDir
      let sver = showVersion ver
      -- TODO: dumping this to stderr is not really acceptable, we need to have
      -- a way to let API clients override this!
      hPutStr stderr $ printf "\
\cabal-helper: Installing a private copy of Cabal because we couldn't\n\
\find the right version anywhere on your system. You can set the environment\n\
\variable CABAL_HELPER_DEBUG=1 to see where we searched.\n\
\\n\
\Note that this installation might take a little while but it will only\n\
\happen once per Cabal library version used in your build-plans.\n\
\\n\
\If you want to avoid this automatic installation altogether install\n\
\version %s of the Cabal library manually, either using cabal or your\n\
\system package manager. With cabal you can use the following command:\n\
\    $ cabal install Cabal --constraint \"Cabal == %s\"\n\
\\n\
\FYI the build products and cabal-helper executable cache are all in the\n\
\following directory, you can simply delete it if you think something\n\
\is broken :\n\
\    %s\n\
\Please do report any problems you encounter.\n\
\\n\
\Installing Cabal %s ...\n" sver sver appdir sver

callCabalInstall
    :: Env
    => PackageDbDir
    -> CabalSourceDir
    -> GhcVersion
    -> UnpackedCabalVersion
    -> IO ()
callCabalInstall
  (PackageDbDir db)
  (CabalSourceDir srcdir)
  ghcVer
  unpackedCabalVer
  = do
  civ@CabalInstallVersion {..} <- cabalInstallVersion
  cabal_opts <- return $ concat
      [
        [ "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--prefix=" ++ db </> "prefix"
        ]
        , cabalWithGHCProgOpts
        , if cabalInstallVer >= Version [1,20,0,0] []
             then ["--no-require-sandbox"]
             else []
        , [ "install", srcdir ]
        , if | ?verbose 3 -> ["-v2"]
             | ?verbose 4 -> ["-v3"]
             | otherwise -> []
        , [ "--only-dependencies" ]
      ]

  callProcessStderr (Just "/") [] (cabalProgram ?progs) cabal_opts

  runSetupHs ghcVer db srcdir unpackedCabalVer civ

  hPutStrLn stderr "done"

runSetupHs
    :: Env
    => GhcVersion
    -> FilePath
    -> FilePath
    -> UnpackedCabalVersion
    -> CabalInstallVersion
    -> IO ()
runSetupHs ghcVer db srcdir cabalVer CabalInstallVersion {..}
    | cabalInstallVer >= parseVer "1.24" = do
      go $ \args -> callProcessStderr (Just srcdir) [] (cabalProgram ?progs) $
        [ "act-as-setup", "--" ] ++ args
    | otherwise = do
      SetupProgram {..} <- compileSetupHs ghcVer db srcdir
      go $ callProcessStderr (Just srcdir) [] setupProgram
  where
    parmake_opt :: Maybe Int -> [String]
    parmake_opt nproc'
        | CabalHEAD _ <- cabalVer =
            ["-j"++nproc]
        | CabalVersion ver <- cabalVer, ver >= Version [1,20] [] =
            ["-j"++nproc]
        | otherwise =
            []
      where
        nproc = fromMaybe "" $ show <$> nproc'
    go :: ([String] -> IO ()) -> IO ()
    go run = do
      run $ [ "configure", "--package-db", db, "--prefix", db </> "prefix" ]
              ++ cabalWithGHCProgOpts
      mnproc <- join . fmap readMaybe <$> lookupEnv "NPROC"
      run $ [ "build" ] ++ parmake_opt mnproc
      run [ "copy" ]
      run [ "register" ]

newtype SetupProgram = SetupProgram { setupProgram :: FilePath }
compileSetupHs :: Env => GhcVersion -> FilePath -> FilePath -> IO SetupProgram
compileSetupHs (GhcVersion ghcVer) db srcdir = do
  let no_version_macros
        | ghcVer >= Version [8] [] = [ "-fno-version-macros" ]
        | otherwise                = []

      file = srcdir </> "Setup"

  callProcessStderr (Just srcdir) [] (ghcProgram ?progs) $ concat
    [ [ "--make"
      , "-package-conf", db
      ]
    , no_version_macros
    , [ file <.> "hs"
      , "-o", file
      ]
    ]
  return $ SetupProgram file

cabalWithGHCProgOpts :: Progs => [String]
cabalWithGHCProgOpts = concat
  [ [ "--with-ghc=" ++ ghcProgram ?progs ]
  -- Only pass ghc-pkg if it was actually set otherwise we
  -- might break cabal's guessing logic
  , if ghcPkgProgram ?progs /= ghcPkgProgram defaultPrograms
      then [ "--with-ghc-pkg=" ++ ghcPkgProgram ?progs ]
      else []
  ]

installCabalLibV2 :: Env => GhcVersion -> UnpackedCabalVersion -> PackageEnvFile -> IO ()
installCabalLibV2 _ghcVer cv (PackageEnvFile env_file) = do
  exists <- doesFileExist env_file
  if exists
    then return ()
    else do
    installingMessage cv
    (target, cwd) <- case cv of
      CabalVersion cabalVer -> do
        tmp <- getTemporaryDirectory
        return $ ("Cabal-"++showVersion cabalVer, tmp)
      CabalHEAD (_commitid, CabalSourceDir srcdir) -> do
        return (".", srcdir)
    CabalInstallVersion {..} <- cabalInstallVersion
    cabal_opts <- return $ concat
        [ if cabalInstallVer >= Version [1,20] []
             then ["--no-require-sandbox"]
             else []
        , [ if cabalInstallVer >= Version [2,4] []
              then "v2-install"
              else "new-install"
          ]
        , cabalV2WithGHCProgOpts
        , [ "--package-env=" ++ env_file
          , "--lib"
          , target
          ]
        , if | ?verbose 3 -> ["-v2"]
             | ?verbose 4 -> ["-v3"]
             | otherwise -> []
        ]
    callProcessStderr (Just cwd) [] (cabalProgram ?progs) cabal_opts
    hPutStrLn stderr "done"


cabalV2WithGHCProgOpts :: Progs => [String]
cabalV2WithGHCProgOpts = concat
  [ [ "--with-compiler=" ++ ghcProgram ?progs ]
  , if ghcPkgProgram ?progs /= ghcPkgProgram defaultPrograms
      then [ "--with-hc-pkg=" ++ ghcPkgProgram ?progs ]
      else []
  ]

planPackages :: CP.PlanJson -> IO [Package ('Cabal 'CV2)]
planPackages plan = do
    fmap Map.elems $
      mapM mkPackage $
      groupByMap $ Map.elems $
      Map.filter ((==CP.UnitTypeLocal) . CP.uType) $
      CP.pjUnits plan
  where
    groupByMap = Map.fromListWith (<>) . map (CP.uPId &&& (:|[]))

    mkPackage :: NonEmpty CP.Unit -> IO (Package ('Cabal 'CV2))
    mkPackage units@(unit :| _) =
      case unit of
       CP.Unit
        { uPkgSrc=Just (CP.LocalUnpackedPackage pkgdir)
        } -> do
          cabal_file <- Cabal.complainIfNoCabalFile pkgdir =<< Cabal.findCabalFile pkgdir
          let pkg = Package
                { pPackageName =
                    let CP.PkgId (CP.PkgName pkg_name) _ = CP.uPId unit
                    in Text.unpack pkg_name
                , pSourceDir = pkgdir
                , pCabalFile = CabalFile cabal_file
                , pFlags = []
                , pUnits = fmap (\u -> fixBackpackUnit u $ mkUnit pkg { pUnits = () } u) units
                }
          return pkg
       _ -> panicIO "planPackages.mkPackage: Got non-unpacked package src!"

    takeBackpackIndefUnitId :: CP.Unit -> Maybe CP.UnitId
    takeBackpackIndefUnitId CP.Unit {uId=CP.UnitId uid}
      | Text.any (=='+') uid = Just $ CP.UnitId $ Text.takeWhile (/='+') uid
      | otherwise = Nothing

    findUnitsDependingOn :: CP.UnitId -> [CP.Unit]
    findUnitsDependingOn uid = Map.elems $
      Map.filter (any (Set.member uid . CP.ciLibDeps) . Map.elems . CP.uComps) $
      CP.pjUnits plan

    -- Horrible workaround for https://github.com/haskell/cabal/issues/6201
    fixBackpackUnit plan_unit ch_unit
      | Just indef_uid <- takeBackpackIndefUnitId plan_unit = do
        let deps = findUnitsDependingOn indef_uid
        ch_unit { uImpl = (uImpl ch_unit)
          { uiV2Components = concatMap unitTargets deps
          , uiV2OnlyDependencies = True
          } }
      | otherwise =
        ch_unit

    unitTargets :: CP.Unit -> [String]
    unitTargets CP.Unit {uComps, uPId=CP.PkgId pkg_name _} =
      map (Text.unpack . (((coerce pkg_name) <> ":") <>) . CP.dispCompNameTarget pkg_name) $
      Map.keys uComps

    mkUnit :: Package' () -> CP.Unit -> Unit ('Cabal 'CV2)
    mkUnit pkg u@CP.Unit
      { uDistDir=Just distdirv1
      , uComps=comps
      , uPId=CP.PkgId pkg_name _
      , uId
      } =
        Unit
          { uUnitId     = UnitId $ Text.unpack (coerce uId)
          , uPackage    = pkg
          , uDistDir    = DistDirLib distdirv1
          , uImpl       =
            let
              comp_names = Map.keys comps
              uiV2ComponentNames = map cpCompNameToChComponentName comp_names
              uiV2Components = unitTargets u
              uiV2OnlyDependencies = False
            in UnitImplV2 {..}
          }
    mkUnit _ _ =
      error "planPackages.mkUnit: Got package without distdir!"

cpCompNameToChComponentName :: CP.CompName -> ChComponentName
cpCompNameToChComponentName cn =
    case cn of
      CP.CompNameSetup         -> ChSetupHsName
      CP.CompNameLib           -> ChLibName     ChMainLibName
      (CP.CompNameSubLib name) -> ChLibName   $ ChSubLibName $ Text.unpack name
      (CP.CompNameFLib name)   -> ChFLibName  $ Text.unpack name
      (CP.CompNameExe name)    -> ChExeName   $ Text.unpack name
      (CP.CompNameTest name)   -> ChTestName  $ Text.unpack name
      (CP.CompNameBench name)  -> ChBenchName $ Text.unpack name

data CabalInstallCommand
    = CIConfigure
    | CIBuild

doCabalInstallCmd
    :: (QueryEnvI c ('Cabal cpt) -> CallProcessWithCwdAndEnv a)
    -> QueryEnvI c ('Cabal cpt)
    -> Maybe FilePath -> CabalInstallCommand -> [String] -> IO a
doCabalInstallCmd procfn qe mcwd cmd args = do
  case (cmd, projTypeOfQueryEnv qe) of
    (CIConfigure, SCabal SCV1) ->
      run "v1-configure" cabalProjArgs cabalUnitArgs []
    (CIBuild, SCabal SCV1) ->
      run "v1-build" cabalProjArgs [] []
    (_, SCabal SCV2) ->
      run "v2-build" cabalProjArgs cabalUnitArgs []
  where
    Programs{..} = qePrograms qe
    run cmdarg before aftercmd after  = procfn qe mcwd [] cabalProgram $
      before ++ [cmdarg] ++ aftercmd ++ args ++ after

readCabalInstallCmd
    :: QueryEnvI c ('Cabal cpt)
    -> Maybe FilePath -> CabalInstallCommand -> [String] -> IO String
callCabalInstallCmd
    :: QueryEnvI c ('Cabal cpt)
    -> Maybe FilePath -> CabalInstallCommand -> [String] -> IO ()

readCabalInstallCmd = doCabalInstallCmd (\qe -> qeReadProcess qe "")
callCabalInstallCmd = doCabalInstallCmd qeCallProcess

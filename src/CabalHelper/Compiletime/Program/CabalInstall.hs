{-# LANGUAGE DataKinds, MultiWayIf #-}

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
Module      : CabalHelper.Compiletime.Program.Cabal
Description : cabal-install program interface
License     : GPL-3
-}

module CabalHelper.Compiletime.Program.CabalInstall where

import qualified Cabal.Plan as CP
import Control.Monad
import Data.Coerce
import Data.Either
import Data.Maybe
import Data.Version
import System.IO
import System.IO.Temp
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf
import Text.Read
import Text.Show.Pretty

import qualified Data.Map.Strict as Map
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
\find the right version in your global/user package-db. This might take a\n\
\while but will only happen once per Cabal version you're using.\n\
\\n\
\If anything goes horribly wrong just delete this directory and try again:\n\
\    %s\n\
\\n\
\If you want to avoid this automatic installation altogether install\n\
\version %s of Cabal manually (into your user or global package-db):\n\
\    $ cabal install Cabal --constraint \"Cabal == %s\"\n\
\\n\
\Installing Cabal %s ...\n" appdir sver sver sver

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

  callProcessStderr (Just "/") (cabalProgram ?progs) cabal_opts

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
      go $ \args -> callProcessStderr (Just srcdir) (cabalProgram ?progs) $
        [ "act-as-setup", "--" ] ++ args
    | otherwise = do
      SetupProgram {..} <- compileSetupHs ghcVer db srcdir
      go $ callProcessStderr (Just srcdir) setupProgram
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

  callProcessStderr (Just srcdir) (ghcProgram ?cprogs) $ concat
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
  [ [ "--with-ghc=" ++ ghcProgram ?cprogs ]
  -- Only pass ghc-pkg if it was actually set otherwise we
  -- might break cabal's guessing logic
  , if ghcPkgProgram ?cprogs /= ghcPkgProgram defaultCompPrograms
      then [ "--with-ghc-pkg=" ++ ghcPkgProgram ?cprogs ]
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
    callProcessStderr (Just cwd) (cabalProgram ?progs) cabal_opts
    hPutStrLn stderr "done"


cabalV2WithGHCProgOpts :: Progs => [String]
cabalV2WithGHCProgOpts = concat
  [ [ "--with-compiler=" ++ ghcProgram ?cprogs ]
  , if ghcPkgProgram ?cprogs /= ghcPkgProgram defaultCompPrograms
      then [ "--with-hc-pkg=" ++ ghcPkgProgram ?cprogs ]
      else []
  ]

planUnits :: CP.PlanJson -> IO [Unit 'V2]
planUnits plan = do
    units <- fmap catMaybes $ mapM takeunit $ Map.elems $ CP.pjUnits plan
    case lefts units of
      [] -> return $ rights units
      us@(_:_) -> panicIO $
        msg ++ (concat $ map (unlines . map ("  "++) . lines . ppShow) us)
  where
    msg = "\
\plan.json doesn't contain 'dist-dir' key for the following local units:\n"
    takeunit u@CP.Unit
      { uType=CP.UnitTypeLocal
      , uDistDir=Just distdirv1
      , uPkgSrc=Just (CP.LocalUnpackedPackage pkgdir)
      , uComps=comps
      , uPId=CP.PkgId pkg_name _
      } = do
        cabal_file <- Cabal.complainIfNoCabalFile pkgdir =<< Cabal.findCabalFile pkgdir
        let comp_names = Map.keys comps
        let uiV2Components =
              map (Text.unpack . CP.dispCompNameTarget pkg_name) $ Map.keys comps
        let uiV2ComponentNames = map cpCompNameToChComponentName comp_names
        return $ Just $ Right $ Unit
          { uUnitId     = UnitId $ Text.unpack (coerce (CP.uId u))
          , uPackageDir = pkgdir
          , uCabalFile  = CabalFile cabal_file
          , uDistDir    = DistDirLib distdirv1
          , uImpl       = UnitImplV2 {..}
          }
    takeunit u@CP.Unit {uType=CP.UnitTypeLocal} =
      return $ Just $ Left u
    takeunit _ =
      return $ Nothing

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

{-# LANGUAGE DataKinds #-}

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
import Control.Arrow
import Control.Monad
import Data.Coerce
import Data.Either
import Data.Maybe
import Data.Version
import System.IO
import System.IO.Temp
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
  ( ghcVersion, createPkgDb )
import CabalHelper.Compiletime.Cabal
  ( CabalSourceDir(..), CabalVersion(..), unpackCabalHEAD, unpackPatchedCabal )
import CabalHelper.Compiletime.Process
import CabalHelper.Shared.Common
  ( parseVer, trim, appCacheDir, panicIO )

newtype CabalInstallVersion = CabalInstallVersion { cabalInstallVer :: Version }

data HEAD = HEAD deriving (Eq, Show)

cabalInstallVersion :: (Verbose, Progs) => IO CabalInstallVersion
cabalInstallVersion = do
  CabalInstallVersion . parseVer . trim
    <$> readProcess' (cabalProgram ?progs) ["--numeric-version"] ""

installCabalLib :: Env => Either HEAD Version -> IO (PackageDbDir, CabalVersion)
installCabalLib ever = do
  appdir <- appCacheDir
  let message ver = do
      let sver = showVersion ver
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
  withSystemTempDirectory "cabal-helper-Cabal-source" $ \tmpdir -> do
    (srcdir, cabalVer) <- case ever of
      Left HEAD -> do
        second CabalHEAD <$> unpackCabalHEAD tmpdir
      Right ver -> do
        message ver
        (,) <$> unpackPatchedCabal ver tmpdir <*> pure (CabalVersion ver)

    db <- createPkgDb cabalVer

    callCabalInstall db srcdir ever

    return (db, cabalVer)

callCabalInstall
    :: Env => PackageDbDir -> CabalSourceDir -> Either HEAD Version-> IO ()
callCabalInstall (PackageDbDir db) (CabalSourceDir srcdir) ever = do
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
        , if ?verbose
            then ["-v"]
            else []
        , [ "--only-dependencies" ]
      ]

  callProcessStderr (Just "/") (cabalProgram ?progs) cabal_opts

  runSetupHs db srcdir ever civ

  hPutStrLn stderr "done"

runSetupHs
    :: Env
    => FilePath
    -> FilePath
    -> Either HEAD Version
    -> CabalInstallVersion
    -> IO ()
runSetupHs db srcdir ever CabalInstallVersion {..}
    | cabalInstallVer >= parseVer "1.24" = do
      go $ \args -> callProcessStderr (Just srcdir) (cabalProgram ?progs) $
        [ "act-as-setup", "--" ] ++ args
    | otherwise = do
      SetupProgram {..} <- compileSetupHs db srcdir
      go $ callProcessStderr (Just srcdir) setupProgram
  where
    parmake_opt :: Maybe Int -> [String]
    parmake_opt nproc'
        | Left _ <- ever = ["-j"++nproc]
        | Right ver <- ever,  ver >= Version [1,20] [] = ["-j"++nproc]
        | otherwise = []
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
compileSetupHs :: Env => FilePath -> FilePath -> IO SetupProgram
compileSetupHs db srcdir = do
  ver <- ghcVersion
  let no_version_macros
        | ver >= Version [8] [] = [ "-fno-version-macros" ]
        | otherwise             = []

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
      } = do
        cabal_file <- Cabal.findCabalFile pkgdir
        return $ Just $ Right $ Unit
          { uUnitId     = UnitId $ Text.unpack (coerce (CP.uId u))
          , uPackageDir = pkgdir
          , uCabalFile  = CabalFile cabal_file
          , uDistDir    = DistDirLib distdirv1
          , uUnitInfo   = UnitImplV2{uiV2Component=map (Text.unpack . CP.dispCompName) (Map.keys $ CP.uComps u)}
          }
    takeunit u@CP.Unit {uType=CP.UnitTypeLocal} =
      return $ Just $ Left u
    takeunit _ =
      return $ Nothing

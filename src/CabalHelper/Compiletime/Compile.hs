-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2018  Daniel Gröber <cabal-helper@dxld.at>
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

{-# LANGUAGE DeriveFunctor, GADTs #-}

{-|
Module      : CabalHelper.Compiletime.Compile
Description : Runtime compilation machinery
License     : GPL-3
-}

module CabalHelper.Compiletime.Compile where

import qualified Cabal.Plan as CP
import Cabal.Plan
  ( PlanJson(..), PkgId(..), PkgName(..), Ver(..), uPId)
import Control.Applicative
import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.Version
import Text.Printf
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import Prelude

import qualified Data.Text as Text
import qualified Data.Map.Strict as Map

import Distribution.System
  ( buildPlatform )
import Distribution.Text
  ( display )

import CabalHelper.Compiletime.Cabal
import CabalHelper.Compiletime.Data
import CabalHelper.Compiletime.Log
import CabalHelper.Compiletime.Program.GHC
import CabalHelper.Compiletime.Program.CabalInstall
import CabalHelper.Compiletime.Sandbox
    ( getSandboxPkgDb )
import CabalHelper.Compiletime.Types

import CabalHelper.Shared.Common

data Compile
    = CompileWithCabalSource
      { compCabalSourceDir     :: CabalSourceDir
      , compCabalSourceVersion :: Version
      }
    | CompileWithCabalPackage
      { compPackageDb      :: Maybe PackageDbDir
      , compCabalVersion   :: CabalVersion
      , compPackageDeps    :: [String]
      , compProductTarget  :: CompilationProductScope
      }

data CompPaths = CompPaths
    { compBuildDir:: FilePath
    , compOutDir  :: FilePath
    , compExePath :: FilePath
    }

-- | The Helper executable we produce as a compilation product can either be
-- placed in a per-project location, or a per-user/global location in the user's
-- home directory. This type controls where the compilation process places the
-- executable.
data CompilationProductScope = CPSGlobal | CPSProject

data CompHelperEnv = CompHelperEnv
  { cheCabalVer :: Version
  , chePkgDb    :: Maybe PackageDbDir
  , cheProjDir  :: FilePath
  , cheNewstyle :: Maybe (PlanJson, FilePath)
  , cheCacheDir :: FilePath
  }

compileHelper :: Env => CompHelperEnv -> IO (Either ExitCode FilePath)
compileHelper CompHelperEnv{..}   = do
    ghcVer <- ghcVersion
    Just (prepare, comp) <- runMaybeT $ msum $
      case chePkgDb of
        Nothing ->
          [ compileCabalSource
          , compileNewBuild ghcVer
          , compileSandbox ghcVer
          , compileGlobal
          , MaybeT $ Just <$> compileWithCabalInPrivatePkgDb
          ]
        Just db ->
          [ pure $ (pure (), compileWithPkg (Just db) cheCabalVer CPSProject)
          ]

    appdir <- appCacheDir

    let cp@CompPaths {compExePath} = compPaths appdir cheCacheDir comp
    exists <- doesFileExist compExePath
    if exists
      then do
        vLog $ "helper already compiled, using exe: "++compExePath
        return (Right compExePath)
      else do
        vLog $ "helper exe does not exist, compiling "++compExePath
        prepare >> compile comp cp

  where
   logMsg = "using helper compiled with Cabal from "

-- for relaxed deps: find (sameMajorVersionAs cheCabalVer) . reverse . sort

   -- | Check if this version is globally available
   compileGlobal :: Env => MaybeT IO (IO (), Compile)
   compileGlobal = do
       cabal_versions <- listCabalVersions Nothing
       ver <- MaybeT $ return $ find (== cheCabalVer) cabal_versions
       vLog $ logMsg ++ "user/global package-db"
       return $ (return (), compileWithPkg Nothing ver CPSGlobal)

   -- | Check if this version is available in the project sandbox
   compileSandbox :: Env => Version -> MaybeT IO (IO (), Compile)
   compileSandbox ghcVer = do
       let mdb_path = getSandboxPkgDb (display buildPlatform) ghcVer cheProjDir
       sandbox <- PackageDbDir <$> MaybeT mdb_path
       cabal_versions <- listCabalVersions (Just sandbox)
       ver <- MaybeT $ return $ find (== cheCabalVer) cabal_versions
       vLog $ logMsg ++ "sandbox package-db"
       return $ (return (), compileWithPkg (Just sandbox) ver CPSProject)

   compileNewBuild :: Env => Version -> MaybeT IO (IO (), Compile)
   compileNewBuild ghcVer = do
       (PlanJson {pjUnits}, distdir_newstyle) <- maybe mzero pure cheNewstyle
       let cabal_pkgid =
               PkgId (PkgName (Text.pack "Cabal"))
                        (Ver $ versionBranch cheCabalVer)
           mcabal_unit = listToMaybe $
             Map.elems $ Map.filter (\CP.Unit{..} -> uPId == cabal_pkgid) pjUnits
       CP.Unit {} <- maybe mzero pure mcabal_unit
       let inplace_db_path = distdir_newstyle
             </> "packagedb" </> ("ghc-" ++ showVersion ghcVer)
           inplace_db = PackageDbDir inplace_db_path
       cabal_versions <- listCabalVersions (Just inplace_db)
       ver <- MaybeT $ return $ find (== cheCabalVer) cabal_versions
       vLog $ logMsg ++ "v2-build package-db " ++ inplace_db_path
       return $ (return (), compileWithPkg (Just inplace_db) ver CPSProject)

   -- | Compile the requested Cabal version into an isolated package-db if it's
   -- not there already
   compileWithCabalInPrivatePkgDb :: Env => IO (IO (), Compile)
   compileWithCabalInPrivatePkgDb = do
       db@(PackageDbDir db_path)
           <- getPrivateCabalPkgDb (CabalVersion cheCabalVer)
       vLog $ logMsg ++ "private package-db in " ++ db_path
       return (prepare db, compileWithPkg (Just db) cheCabalVer CPSGlobal)
     where
       prepare db = do
         db_exists <- liftIO $ cabalVersionExistsInPkgDb cheCabalVer db
         when (not db_exists) $
           void $ installCabalLib (Right cheCabalVer) `E.catch`
             \(SomeException _) -> errorInstallCabal cheCabalVer

   -- | See if we're in a cabal source tree
   compileCabalSource :: Env => MaybeT IO (IO (), Compile)
   compileCabalSource = do
       let cabalFile = cheProjDir </> "Cabal.cabal"
       cabalSrc <- liftIO $ doesFileExist cabalFile
       let projdir = CabalSourceDir cheProjDir
       case cabalSrc of
         False -> mzero
         True -> do
           vLog $ "projdir looks like Cabal source tree (Cabal.cabal exists)"
           cf <- liftIO $ readFile cabalFile
           let buildType = cabalFileBuildType cf
               ver       = cabalFileVersion cf

           case buildType of
             "simple" -> do
                 vLog $ "Cabal source tree is build-type:simple, moving on"
                 mzero
             "custom" -> do
                 vLog $ "compiling helper with local Cabal source tree"
                 return $ (return (), compileWithCabalSource projdir ver)
             _ -> error $ "compileCabalSource: unknown build-type: '"++buildType++"'"

   compileWithCabalSource srcDir ver =
       CompileWithCabalSource
          { compCabalSourceDir       = srcDir
          , compCabalSourceVersion   = ver
          }

   compileWithPkg mdb ver target =
       CompileWithCabalPackage
          { compPackageDb            = mdb
          , compCabalVersion         = CabalVersion ver
          , compPackageDeps          = [cabalPkgId ver]
          , compProductTarget        = target
          }

   cabalPkgId v = "Cabal-" ++ showVersion v

compile :: Env => Compile -> CompPaths -> IO (Either ExitCode FilePath)
compile comp paths@CompPaths {..} = do
    createDirectoryIfMissing True compOutDir
    createHelperSources compBuildDir

    vLog $ "compBuildDir: " ++ compBuildDir
    vLog $ "compOutDir: " ++ compOutDir
    vLog $ "compExePath: " ++ compExePath

    invokeGhc $ compGhcInvocation comp paths

compPaths :: FilePath -> FilePath -> Compile -> CompPaths
compPaths appdir cachedir c =
    case c of
      CompileWithCabalPackage {compProductTarget=CPSGlobal,..} -> CompPaths {..}
        where
          compBuildDir = appdir </> exeName compCabalVersion ++ "--" ++ sourceHash <.> "build"
          compOutDir  = compBuildDir
          compExePath = compBuildDir </> "cabal-helper"

      CompileWithCabalPackage {compProductTarget=CPSProject,..} -> cachedirPaths
      CompileWithCabalSource {..} -> cachedirPaths
  where
    cachedirPaths = CompPaths {..}
        where
          compBuildDir = cachedir </> "cabal-helper"
          compOutDir  = compBuildDir
          compExePath = compOutDir </> "cabal-helper"

compGhcInvocation :: Compile -> CompPaths -> GhcInvocation
compGhcInvocation comp CompPaths {..} =
    case comp of
      CompileWithCabalSource {..} ->
        GhcInvocation
          { giIncludeDirs = [compBuildDir, unCabalSourceDir compCabalSourceDir]
          , giPackageDBs  = []
          , giHideAllPackages = False
          , giPackages    = []
          , giCPPOptions = cppOptions compCabalSourceVersion
                           ++ [cabalVersionMacro compCabalSourceVersion]
          , ..
          }
      CompileWithCabalPackage {..} ->
        GhcInvocation
          { giIncludeDirs = [compBuildDir]
          , giPackageDBs = maybeToList compPackageDb
          , giHideAllPackages = True
          , giPackages =
              [ "base"
              , "containers"
              , "directory"
              , "filepath"
              , "process"
              , "bytestring"
              , "ghc-prim"
              ] ++ compPackageDeps
          , giCPPOptions = cppOptions (unCabalVersion compCabalVersion)
          , ..
          }
  where

    unCabalVersion (CabalVersion ver) = ver
    unCabalVersion (CabalHEAD _)      = Version [10000000, 0, 0] []

    cppOptions cabalVer =
        [ "-DCABAL_HELPER=1"
        , cabalMinVersionMacro cabalVer
        ]

    giOutDir = compOutDir
    giOutput = compExePath
    giWarningFlags = [ "-w" ] -- no point in bothering end users with warnings
    giInputs = [compBuildDir</>"CabalHelper"</>"Runtime"</>"Main.hs"]

cabalVersionMacro :: Version -> String
cabalVersionMacro (Version vs _) =
  "-DCABAL_VERSION="++intercalate "," (map show vs)

cabalMinVersionMacro :: Version -> String
cabalMinVersionMacro (Version (mj1:mj2:mi:_) _) =
  "-DCH_MIN_VERSION_Cabal(major1,major2,minor)=\
  \(  (major1)  < "++show mj1++" \
  \|| (major1) == "++show mj1++" && (major2)  < "++show mj2++" \
  \|| (major1) == "++show mj1++" && (major2) == "++show mj2++" && (minor) <= "++show mi++
  ")"
cabalMinVersionMacro _ =
    error "cabalMinVersionMacro: Version must have at least 3 components"

{-
TODO: If the Cabal version we want to install is less than or equal to one we
have available, either through act-as-setup or in a package-db we should be able
to use act-as-setup or build a default Setup.hs exe and patch the Cabal source
to say build-type:simple. This will sidestep bugs in c-i>=1.24

See conversation in
https://github.com/haskell/cabal/commit/e2bf243300957321497353a2f85517e464f764ab

Otherwise we might be able to use the shipped Setup.hs

-}

errorInstallCabal :: Version -> IO a
errorInstallCabal cabalVer = panicIO $ printf "\
\Installing Cabal version %s failed.\n\
\\n\
\You have the following choices to fix this:\n\
\\n\
\- The easiest way to try and fix this is just reconfigure the project and try\n\
\  again:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If that fails you can try to install the version of Cabal mentioned above\n\
\  into your global/user package-db somehow, you'll probably have to fix\n\
\  something otherwise it wouldn't have failed above:\n\
\        $ cabal install Cabal --constraint 'Cabal == %s'\n\
\\n\
\- If you're using `Build-Type: Simple`:\n\
\  - You can see if you can reinstall your cabal-install executable while\n\
\    having it linked to a version of Cabal that's available in you\n\
\    package-dbs or can be built automatically:\n\
\        $ ghc-pkg list | grep Cabal  # find an available Cabal version\n\
\            Cabal-W.X.Y.Z\n\
\        $ cabal install cabal-install --constraint 'Cabal == W.X.*'\n\
\    Afterwards you'll have to reconfigure your project:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If you're using `Build-Type: Custom`:\n\
\  - Have cabal-install rebuild your Setup.hs executable with a version of the\n\
\    Cabal library that you have available in your global/user package-db:\n\
\        $ cabal clean && cabal configure\n\
\    You might also have to install some version of the Cabal to do this:\n\
\        $ cabal install Cabal\n\
\\n" sver sver
 where
   sver = showVersion cabalVer

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion = parseVer . cabalFileTopField "version"

-- | Find @build-type: XXX@ delcaration in a cabal file
cabalFileBuildType :: String -> String
cabalFileBuildType = cabalFileTopField "build-type"

cabalFileTopField :: String -> String -> String
cabalFileTopField field cabalFile = value
 where
  Just value = extract <$> find ((field++":") `isPrefixOf`) ls
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> drop 1 >>> dropWhile isSpace >>> takeWhile (not . isSpace)

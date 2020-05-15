-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE DeriveFunctor, GADTs, ScopedTypeVariables #-}

{-|
Module      : CabalHelper.Compiletime.Compile
Description : Runtime compilation machinery
License     : Apache-2.0
-}

module CabalHelper.Compiletime.Compile where

import qualified Cabal.Plan as CP
import Cabal.Plan
  ( PkgId(..), PkgName(..), Ver(..), uPId)
import Control.Applicative
import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Map.Strict (Map)
import Data.Maybe
import Data.String
import Data.Version
import Text.Printf
import qualified System.Clock as Clock
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.IO.Temp
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
import CabalHelper.Compiletime.Types.Cabal

import CabalHelper.Shared.Common

import Paths_cabal_helper (version)


data Compile
    = CompileWithCabalSource
      { compCabalSourceDir     :: !CabalSourceDir
      , compCabalSourceVersion :: !Version
      }
    | CompileWithCabalPackage
      { compPackageSource  :: !GhcPackageSource
      , compCabalVersion   :: !ResolvedCabalVersion
      , compProductTarget  :: !CompilationProductScope
      }

data CompPaths = CompPaths
    { compBuildDir:: !FilePath
    , compOutDir  :: !FilePath
    , compExePath :: !FilePath
    }

-- | The Helper executable we produce as a compilation product can either be
-- placed in a per-project location, or a per-user/global location in the user's
-- home directory. This type controls where the compilation process places the
-- executable.
data CompilationProductScope = CPSGlobal | CPSProject

type CompHelperEnv = CompHelperEnv' CabalVersion
data CompHelperEnv' cv = CompHelperEnv
  { cheCabalVer :: !cv
  , chePkgDb    :: ![PackageDbDir]
  -- ^ A package-db where we are guaranteed to find Cabal-`cheCabalVer`.
  , cheProjDir  :: !FilePath
  , chePjUnits  :: !(Maybe (Map CP.UnitId CP.Unit))
  , cheDistV2   :: !(Maybe FilePath)
  , cheProjLocalCacheDir :: FilePath
  }

compileHelper
    :: Env => CompHelperEnv -> IO (Either ExitCode FilePath)
compileHelper che@CompHelperEnv {cheCabalVer} = do
  withSystemTempDirectory "cabal-helper.compile-tmp" $ \tmpdir -> do
    ucv <- unpackCabal cheCabalVer tmpdir
    compileHelper' che { cheCabalVer = ucv }

compileHelper'
    :: Env
    => CompHelperEnv' UnpackedCabalVersion
    -> IO (Either ExitCode FilePath)
compileHelper' CompHelperEnv {..} = do
  ghcVer <- ghcVersion
  Just (prepare, comp) <- case cheCabalVer of
    cabalVer@CabalHEAD {} -> runMaybeT $ msum  $ map (\f -> f ghcVer cabalVer)
      [ compileWithCabalV2GhcEnv'
      , compileWithCabalInPrivatePkgDb'
      ]
    CabalVersion cabalVerPlain -> do
      runMaybeT $ msum $ map (\f -> f ghcVer cabalVerPlain) $
        case chePkgDb of
          [] ->
            [ compileWithCabalV2Inplace
            , compileWithCabalV2GhcEnv
            , compileCabalSource
            , compileSandbox
            , compileGlobal
            , compileWithCabalInPrivatePkgDb
            ]
          dbs ->
            [ ((.).(.)) liftIO (compilePkgDbs dbs)
            ]
  appdir <- appCacheDir
  let cp@CompPaths {compExePath} = compPaths appdir cheProjLocalCacheDir comp
  helper_exists <- doesFileExist compExePath
  rv <- if helper_exists
    then do
      vLog $ "helper already compiled, using exe: "++compExePath
      return (Right compExePath)
    else do
      vLog $ "helper exe does not exist, compiling "++compExePath
      prepare >> compile cp comp
  return rv


  where
   logMsg = "using helper compiled with Cabal from "

-- for relaxed deps: find (sameMajorVersionAs cheCabalVer) . reverse . sort

   compilePkgDbs dbs _ghcVer cabalVer = return $
       (,)
         (pure ())
         CompileWithCabalPackage
           { compPackageSource = GPSPackageDBs dbs
           , compCabalVersion  = CabalVersion cabalVer
           , compProductTarget = CPSProject
           }

   -- | Check if this version is globally available
   compileGlobal :: Env => gv -> Version -> MaybeT IO (IO (), Compile)
   compileGlobal _ghcVer cabalVer = do
       cabal_versions <- listCabalVersions Nothing
       _ <- MaybeT $ return $ find (== cabalVer) cabal_versions
       vLog $ logMsg ++ "user/global package-db"
       return $ (return (), compileWithPkg GPSAmbient cabalVer CPSGlobal)

   -- | Check if this version is available in the project sandbox
   compileSandbox :: Env => GhcVersion -> Version -> MaybeT IO (IO (), Compile)
   compileSandbox  ghcVer cabalVer = do
       let mdb_path = getSandboxPkgDb (display buildPlatform) ghcVer cheProjDir
       sandbox <- PackageDbDir <$> MaybeT mdb_path
       cabal_versions <- listCabalVersions (Just sandbox)
       _ <- MaybeT $ return $ find (== cabalVer) cabal_versions
       vLog $ logMsg ++ "sandbox package-db"
       return $ (return (), compileWithPkg (GPSPackageDBs [sandbox]) cabalVer CPSProject)

   -- | Check if the requested Cabal version is available in a v2-build
   -- project's inplace package-db.
   --
   -- This is likely only the case if Cabal was vendored by this project or if
   -- we're operating on Cabal itself!
   compileWithCabalV2Inplace :: Env => GhcVersion -> Version -> MaybeT IO (IO (), Compile)
   compileWithCabalV2Inplace ghcVer cabalVer = do
       -- TODO: Test coverage! Neither compile-test nor ghc-session test out
       -- this code path
       pjUnits <- maybe mzero pure chePjUnits
       distdir_newstyle <- maybe mzero pure cheDistV2
       let cabal_pkgid =
             PkgId (PkgName (Text.pack "Cabal")) (Ver $ versionBranch cabalVer)
           mcabal_unit = listToMaybe $
             Map.elems $ Map.filter (\CP.Unit{..} -> uPId == cabal_pkgid) pjUnits
       CP.Unit {} <- maybe mzero pure mcabal_unit
       let inplace_db_path = distdir_newstyle
             </> "packagedb" </> ("ghc-" ++ showGhcVersion ghcVer)
           inplace_db = PackageDbDir inplace_db_path
       cabal_versions <- listCabalVersions (Just inplace_db)
       _ <- MaybeT $ return $ find (== cabalVer) cabal_versions
       vLog $ logMsg ++ "v2-build package-db " ++ inplace_db_path
       return $ (return (), compileWithPkg (GPSPackageDBs [inplace_db]) cabalVer CPSProject)

   compileWithCabalV2GhcEnv :: Env => GhcVersion -> Version -> MaybeT IO (IO (), Compile)
   compileWithCabalV2GhcEnv ghcVer cabalVer =
     compileWithCabalV2GhcEnv' ghcVer (CabalVersion cabalVer)

   -- TODO: Support using existing ghc-environments too! That's mostly
   -- relevant for when you want to use a development version of
   -- cabal-install since that will depend on an unreleased version of
   -- Cabal we cannot new-install just like that.

   -- | If this is a v2-build project it makes sense to use @v2-install@ for
   -- installing Cabal as this will use the @~/.cabal/store@. We use
   -- @--package-env@ to instruct cabal to not meddle with the user's package
   -- environment.
   compileWithCabalV2GhcEnv' :: Env => GhcVersion -> UnpackedCabalVersion -> MaybeT IO (IO (), Compile)
   compileWithCabalV2GhcEnv' ghcVer cabalVer = do
       _ <- maybe mzero pure cheDistV2 -- bail if this isn't a v2-build project
       CabalInstallVersion instVer <- liftIO cabalInstallVersion
       guard $ instVer >= (Version [2,4,1,0] [])
       --  ^ didn't test with older versions
       guard $ ghcVer  >= (GhcVersion (Version [8,0] []))
       env@(PackageEnvFile env_file) <- liftIO $
         getPrivateCabalPkgEnv ghcVer $ unpackedToResolvedCabalVersion cabalVer
       vLog $ logMsg ++ "v2-build package-env " ++ env_file
       return $ (,)
         (prepare env)
         CompileWithCabalPackage
           { compPackageSource = GPSPackageEnv env
           , compCabalVersion  = unpackedToResolvedCabalVersion cabalVer
           , compProductTarget = CPSGlobal
           }
     where
       prepare env = do
         -- exists_in_env <- liftIO $ cabalVersionExistsInPkgDb cheCabalVer db
         void $ installCabalLibV2 ghcVer cheCabalVer env `E.catch`
           \(ex :: IOError) -> print ex >>
               case cheCabalVer of
                 CabalHEAD _ -> panicIO "Installing Cabal HEAD failed."
                 CabalVersion ver -> errorInstallCabal (CabalVersion ver)



   compileWithCabalInPrivatePkgDb
       :: (Env, MonadIO m) => GhcVersion -> Version -> m (IO (), Compile)
   compileWithCabalInPrivatePkgDb ghcVer cabalVer =
       compileWithCabalInPrivatePkgDb' ghcVer (CabalVersion cabalVer)

   -- | Compile the requested Cabal version into an isolated package-db if it's
   -- not there already
   compileWithCabalInPrivatePkgDb'
       :: (Env, MonadIO m) => GhcVersion -> UnpackedCabalVersion -> m (IO (), Compile)
   compileWithCabalInPrivatePkgDb' ghcVer cabalVer = do
       db@(PackageDbDir db_path) <- liftIO $
         getPrivateCabalPkgDb $ unpackedToResolvedCabalVersion cabalVer
       vLog $ logMsg ++ "private package-db in " ++ db_path
       return $ (,)
         (prepare db)
         CompileWithCabalPackage
           { compPackageSource = GPSPackageDBs [db]
           , compCabalVersion  = unpackedToResolvedCabalVersion cabalVer
           , compProductTarget = CPSGlobal
           }
     where
       prepare db = do
         db_exists <- liftIO $ cabalVersionExistsInPkgDb cabalVer db
         when (not db_exists) $
           void (installCabalLibV1 ghcVer cabalVer) `E.catch`
             \(SomeException _) -> errorInstallCabal cabalVer

   -- | See if we're in a cabal source tree
   --   compileCabalSource :: Env => MaybeT IO (IO (), Compile)
   compileCabalSource _ghcVer _cabalVer = do
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

   compileWithPkg pkg_src ver target =
       CompileWithCabalPackage
          { compPackageSource        = pkg_src
          , compCabalVersion         = CabalVersion ver
          , compProductTarget        = target
          }

compile :: Env => CompPaths -> Compile -> IO (Either ExitCode FilePath)
compile paths@CompPaths {..} comp = do
    createDirectoryIfMissing True compOutDir
    createHelperSources compBuildDir

    vLog $ "compBuildDir: " ++ compBuildDir
    vLog $ "compOutDir: " ++ compOutDir
    vLog $ "compExePath: " ++ compExePath

    invokeGhc $ compGhcInvocation comp paths

compPaths :: FilePath -> FilePath -> Compile -> CompPaths
compPaths appdir proj_local_cachedir c =
  case c of
    CompileWithCabalPackage
      { compProductTarget=CPSGlobal
      , compCabalVersion
      } -> CompPaths {..}
        where
          compBuildDir =
            appdir </> exeName compCabalVersion ++ "--" ++ sourceHash <.> "build"
          compOutDir  = compBuildDir
          compExePath = compBuildDir </> "cabal-helper" <.> exeExtension
    CompileWithCabalPackage {compProductTarget=CPSProject} ->
        projLocalCachedirPaths
    CompileWithCabalSource {} ->
        projLocalCachedirPaths
  where
    projLocalCachedirPaths = CompPaths {..}
        where
          compBuildDir = proj_local_cachedir </> "cabal-helper"
          compOutDir  = compBuildDir
          compExePath = compOutDir </> "cabal-helper" <.> exeExtension

exeName :: ResolvedCabalVersion -> String
exeName (CabalHEAD commitid) = intercalate "--"
  [ "cabal-helper-" ++ showVersion version
  , "Cabal-HEAD" ++ unCommitId commitid
  ]
exeName CabalVersion {cvVersion} = intercalate "--"
  [ "cabal-helper-" ++ showVersion version
  , "Cabal-" ++ showVersion cvVersion
  ]

compGhcInvocation :: Compile -> CompPaths -> GhcInvocation
compGhcInvocation comp CompPaths {..} =
    case comp of
      CompileWithCabalSource {..} ->
        GhcInvocation
          { giIncludeDirs = [compBuildDir, unCabalSourceDir compCabalSourceDir]
          , giPackageSource = GPSAmbient
          , giHideAllPackages = False
          , giPackages    = []
          , giCPPOptions = cppOptions compCabalSourceVersion
                           ++ [cabalVersionMacro compCabalSourceVersion]
          , ..
          }
      CompileWithCabalPackage {..} ->
        GhcInvocation
          { giIncludeDirs = [compBuildDir]
          , giPackageSource = compPackageSource
          , giHideAllPackages = True
          , giPackages =
              [ "base"
              , "containers"
              , "directory"
              , "filepath"
              , "process"
              , "bytestring"
              , "ghc-prim"
              , case compCabalVersion of
                  CabalHEAD {} -> "Cabal"
                  CabalVersion ver -> "Cabal-" ++ showVersion ver
              ]
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

errorInstallCabal :: CabalVersion' a -> IO b
errorInstallCabal (CabalHEAD _) =
  error "cabal-helper: Installing Cabal HEAD failed."
errorInstallCabal (CabalVersion cabalVer) = panicIO $ printf "\
\cabal-helper: Installing Cabal version %s failed.\n\
\\n\
\You have the following choices to fix this:\n\
\\n\
\- The easiest way to try and fix this is just reconfigure the project and try\n\
\  again:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If that fails you can try to install the version of Cabal mentioned above\n\
\  into your global/user package-db somehow, though you'll probably have to\n\
\  fix something otherwise it wouldn't have failed above:\n\
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

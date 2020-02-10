{-# LANGUAGE GADTs #-}

module CabalHelper.Compiletime.CompPrograms where

import Control.Monad (when)
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.IO.Temp

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Cabal (getCabalVerbosity)
import CabalHelper.Shared.Common (panicIO)
import Symlink (createSymbolicLink)

import Distribution.Simple.GHC as GHC (configure)

import qualified Distribution.Simple.Program as ProgDb
  ( lookupProgram, lookupKnownProgram, programPath
  , configureProgram, userMaybeSpecifyPath
  , ghcProgram, ghcPkgProgram, haddockProgram )
import qualified Distribution.Simple.Program.Db as ProgDb

-- | Determine ghc-pkg/haddock path from ghc path
guessCompProgramPaths :: Verbose => Programs -> IO Programs
guessCompProgramPaths progs = do
  let v = getCabalVerbosity
      getMaybeProg' = getMaybeProg progs
      progdb =
        ProgDb.userMaybeSpecifyPath "ghc" (getMaybeProg' ghcProgram) $
        ProgDb.userMaybeSpecifyPath "ghc-pkg" (getMaybeProg' ghcPkgProgram) $
        ProgDb.userMaybeSpecifyPath "haddock" (getMaybeProg' haddockProgram) $
        ProgDb.defaultProgramDb
  (_compiler, _mplatform, progdb1) <- GHC.configure v Nothing Nothing progdb
  let Just haddockKnownProgram = ProgDb.lookupKnownProgram "haddock" progdb1
  progdb2 <- ProgDb.configureProgram v haddockKnownProgram progdb1
  let getProg p = ProgDb.programPath <$> ProgDb.lookupProgram p progdb2
  return progs
    { ghcProgram =
        fromMaybe (ghcProgram progs) $ getProg ProgDb.ghcProgram
    , ghcPkgProgram =
        fromMaybe (ghcPkgProgram progs) $ getProg ProgDb.ghcPkgProgram
    , haddockProgram =
        fromMaybe (haddockProgram progs) $ getProg ProgDb.haddockProgram
    }

getMaybeProg :: Programs -> (Programs -> FilePath) -> Maybe FilePath
getMaybeProg progs fn
    | fn progs == fn defaultPrograms = Nothing
    | otherwise = Just (fn progs)

patchBuildToolProgs :: SProjType pt -> Programs -> IO Programs
patchBuildToolProgs (SCabal _) progs = return progs
  { cabalUnitArgs = concat
    [ maybeToList (("--with-ghc="++) <$> getMaybeProg progs ghcProgram)
    , maybeToList (("--with-ghc-pkg="++) <$> getMaybeProg progs ghcPkgProgram)
    , maybeToList (("--with-haddock="++) <$> getMaybeProg progs haddockProgram)
    ] ++ cabalUnitArgs progs
  }
patchBuildToolProgs SStack progs
  -- optimization; if none of the program paths are non-default we don't
  -- even have to add anything to PATH.
  | ghcProgram progs == "ghc"
  , ghcPkgProgram progs == "ghc-pkg"
  , haddockProgram progs == "haddock"
  = return progs

  -- optimization; if all paths are unqualified and have the same version
  -- postfix Stack's default behaviour works for us.
  | [ghc] <- splitPath (ghcProgram progs)
  , [ghcPkg] <- splitPath (ghcPkgProgram progs)
  , [haddock] <- splitPath (haddockProgram progs)
  , Just ver <- stripPrefix "ghc-" ghc
  , Just ver == stripPrefix "ghc-pkg-" ghcPkg
  , Just ver == stripPrefix "haddock-" haddock
  = return progs
patchBuildToolProgs SStack progs = do
  -- otherwise fall back to creating a symlink farm
  --
  -- This is of course all quite horrible and we would much prefer just
  -- being able to pass executable paths straight through to stack but
  -- currently there is no option to let us do that.
  withSystemTempDirectory "cabal-helper-symlinks" $ \bindir -> do
  createProgSymlink True bindir $ ghcProgram progs
  createProgSymlink True bindir $ ghcPkgProgram progs
  createProgSymlink False bindir $ haddockProgram progs
  return $ progs
    { stackEnv =
        [("PATH", EnvPrepend $ bindir ++ [searchPathSeparator])] ++
        stackEnv progs
    }

createProgSymlink :: Bool -> FilePath -> FilePath -> IO ()
createProgSymlink required bindir target
  | [exe] <- splitPath target = do
    mb_exe_path <- findExecutable exe
    case mb_exe_path of
      Just exe_path -> createSymbolicLink exe_path (bindir </> takeFileName target)
      Nothing -> when required $ panicIO $ "Error trying to create symlink to '" ++ target ++ "': "
                                        ++ "'" ++ exe ++ "'" ++ " executable not found."
  | otherwise = do
    cwd <- getCurrentDirectory
    createSymbolicLink (cwd </> target) (bindir </> takeFileName target)

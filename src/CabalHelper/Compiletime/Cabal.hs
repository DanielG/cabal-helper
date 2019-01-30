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
Description : Cabal library source unpacking
License     : GPL-3
-}

{-# LANGUAGE DeriveFunctor #-}

module CabalHelper.Compiletime.Cabal where

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.Version
import System.Exit
import System.Directory
import System.FilePath

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Process
import CabalHelper.Shared.Common (trim, replace)

type UnpackedCabalVersion = CabalVersion' (CommitId, CabalSourceDir)
type ResolvedCabalVersion = CabalVersion' CommitId
type CabalVersion = CabalVersion' ()

unpackedToResolvedCabalVersion :: UnpackedCabalVersion -> ResolvedCabalVersion
unpackedToResolvedCabalVersion (CabalHEAD (commit, _)) = CabalHEAD commit
unpackedToResolvedCabalVersion (CabalVersion ver) = CabalVersion ver

-- | Cabal library version we're compiling the helper exe against.
data CabalVersion' a
    = CabalHEAD a
    | CabalVersion { cvVersion :: Version }
      deriving (Eq, Ord, Functor)

newtype CommitId = CommitId { unCommitId :: String }

showUnpackedCabalVersion :: UnpackedCabalVersion -> String
showUnpackedCabalVersion (CabalHEAD (commitid, _)) =
  "HEAD-" ++ unCommitId commitid
showUnpackedCabalVersion CabalVersion {cvVersion} =
  showVersion cvVersion

showResolvedCabalVersion :: ResolvedCabalVersion -> String
showResolvedCabalVersion (CabalHEAD commitid) =
  "HEAD-" ++ unCommitId commitid
showResolvedCabalVersion CabalVersion {cvVersion} =
  showVersion cvVersion

showCabalVersion :: CabalVersion -> String
showCabalVersion (CabalHEAD ()) =
  "HEAD"
showCabalVersion CabalVersion {cvVersion} =
  showVersion cvVersion

data CabalPatchDescription = CabalPatchDescription
  { cpdVersions      :: [Version]
  , cpdUnpackVariant :: UnpackCabalVariant
  , cpdPatchFn       :: FilePath -> IO ()
  }

nopCabalPatchDescription :: CabalPatchDescription
nopCabalPatchDescription =
  CabalPatchDescription [] LatestRevision (const (return ()))

patchyCabalVersions :: [CabalPatchDescription]
patchyCabalVersions = [
  let versions  = [ Version [1,18,1] [] ]
      variant   = Pristine
      patch     = fixArrayConstraint
  in CabalPatchDescription versions variant patch,

  let versions  = [ Version [1,18,0] [] ]
      variant   = Pristine
      patch dir = do
        fixArrayConstraint dir
        fixOrphanInstance dir
  in CabalPatchDescription versions variant patch,

  let versions  = [ Version [1,24,1,0] [] ]
      variant   = Pristine
      patch _   = return ()
  in CabalPatchDescription versions variant patch
  ]
 where
   fixArrayConstraint dir = do
     let cabalFile    = dir </> "Cabal.cabal"
         cabalFileTmp = cabalFile ++ ".tmp"

     cf <- readFile cabalFile
     writeFile cabalFileTmp $ replace "&& < 0.5" "&& < 0.6" cf
     renameFile cabalFileTmp cabalFile

   fixOrphanInstance dir = do
     let versionFile    = dir </> "Distribution/Version.hs"
         versionFileTmp = versionFile ++ ".tmp"

     let languagePragma =
           "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}"
         languagePragmaCPP =
           "{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving #-}"

         derivingDataVersion =
           "deriving instance Data Version"
         derivingDataVersionCPP = unlines [
             "#if __GLASGOW_HASKELL__ < 707",
             derivingDataVersion,
             "#endif"
           ]

     vf <- readFile versionFile
     writeFile versionFileTmp
       $ replace derivingDataVersion derivingDataVersionCPP
       $ replace languagePragma languagePragmaCPP vf

     renameFile versionFileTmp versionFile

unpackPatchedCabal :: Env => Version -> FilePath -> IO CabalSourceDir
unpackPatchedCabal cabalVer tmpdir = do
    res@(CabalSourceDir dir) <- unpackCabalHackage cabalVer tmpdir variant
    patch dir
    return res
  where
    CabalPatchDescription _ variant patch = fromMaybe nopCabalPatchDescription $
      find ((cabalVer `elem`) . cpdVersions) patchyCabalVersions

-- legacy, for `installCabalLib` v1
unpackCabalV1
  :: Env
  => UnpackedCabalVersion
  -> FilePath
  -> IO CabalSourceDir
unpackCabalV1 (CabalVersion ver) tmpdir = do
  csdir <- unpackPatchedCabal ver tmpdir
  return csdir
unpackCabalV1 (CabalHEAD (_commit, csdir)) _tmpdir =
  return csdir

unpackCabal :: Env => CabalVersion -> FilePath -> IO UnpackedCabalVersion
unpackCabal (CabalVersion ver) _tmpdir = do
  return $ CabalVersion ver
unpackCabal (CabalHEAD ()) tmpdir = do
  (commit, csdir) <- unpackCabalHEAD tmpdir
  return $ CabalHEAD (commit, csdir)

data UnpackCabalVariant = Pristine | LatestRevision
newtype CabalSourceDir = CabalSourceDir { unCabalSourceDir :: FilePath }
unpackCabalHackage
    :: (Verbose, Progs)
    => Version
    -> FilePath
    -> UnpackCabalVariant
    -> IO CabalSourceDir
unpackCabalHackage cabalVer tmpdir variant = do
  let cabal = "Cabal-" ++ showVersion cabalVer
      dir = tmpdir </> cabal
      variant_opts = case variant of Pristine -> [ "--pristine" ]; _ -> []
      args = [ "get", cabal ] ++ variant_opts
  callProcessStderr (Just tmpdir) (cabalProgram ?progs) args
  return $ CabalSourceDir dir

unpackCabalHEAD :: Env => FilePath -> IO (CommitId, CabalSourceDir)
unpackCabalHEAD tmpdir = do
  let dir = tmpdir </> "cabal-head.git"
      url = "https://github.com/haskell/cabal.git"
  ExitSuccess <- rawSystem "git" [ "clone", "--depth=1", url, dir]
  commit <-
      withDirectory_ dir $ trim <$> readProcess' "git" ["rev-parse", "HEAD"] ""
  return (CommitId commit, CabalSourceDir $ dir </> "Cabal")
 where
   withDirectory_ :: FilePath -> IO a -> IO a
   withDirectory_ dir action =
       bracket
         (liftIO getCurrentDirectory)
         (liftIO . setCurrentDirectory)
         (\_ -> liftIO (setCurrentDirectory dir) >> action)

resolveCabalVersion :: Verbose => CabalVersion -> IO ResolvedCabalVersion
resolveCabalVersion (CabalVersion ver) = return $ CabalVersion ver
resolveCabalVersion (CabalHEAD ()) = do
  out <- readProcess' "git"
    [ "ls-remote", "https://github.com/haskell/cabal.git", "-h", "master" ] ""
  let commit = takeWhile isHexDigit out
  return $ CabalHEAD $ CommitId commit

findCabalFile :: FilePath -> IO FilePath
findCabalFile pkgdir = do
    [cfile] <- filter isCabalFile <$> getDirectoryContents pkgdir
    return $ pkgdir </> cfile
  where
    isCabalFile :: FilePath -> Bool
    isCabalFile f = takeExtension' f == ".cabal"

    takeExtension' :: FilePath -> String
    takeExtension' p =
        if takeFileName p == takeExtension p
          then "" -- just ".cabal" is not a valid cabal file
          else takeExtension p

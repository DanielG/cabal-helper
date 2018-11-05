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

module CabalHelper.Compiletime.Cabal where

import Control.Exception (bracket)
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Version
import System.Exit
import System.Directory
import System.FilePath

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Process
import CabalHelper.Shared.Common (trim, replace)
import Paths_cabal_helper (version)

-- | Cabal library version we're compiling the helper exe against.
data CabalVersion
    = CabalHEAD { cvCommitId :: CommitId }
    | CabalVersion { cabalVersion :: Version }

newtype CommitId = CommitId { unCommitId :: String }

showCabalVersion :: CabalVersion -> String
showCabalVersion (CabalHEAD commitid) =
  "HEAD-" ++ unCommitId commitid
showCabalVersion CabalVersion {cabalVersion} =
  showVersion cabalVersion

exeName :: CabalVersion -> String
exeName (CabalHEAD commitid) = intercalate "--"
  [ "cabal-helper-" ++ showVersion version
  , "Cabal-HEAD" ++ unCommitId commitid
  ]
exeName CabalVersion {cabalVersion} = intercalate "--"
  [ "cabal-helper-" ++ showVersion version
  , "Cabal-" ++ showVersion cabalVersion
  ]

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
    res@(CabalSourceDir dir) <- unpackCabal cabalVer tmpdir variant
    patch dir
    return res
  where
    CabalPatchDescription _ variant patch = fromMaybe nopCabalPatchDescription $
      find ((cabalVer `elem`) . cpdVersions) patchyCabalVersions


data UnpackCabalVariant = Pristine | LatestRevision
newtype CabalSourceDir = CabalSourceDir { unCabalSourceDir :: FilePath }
unpackCabal
    :: (Verbose, Progs)
    => Version
    -> FilePath
    -> UnpackCabalVariant
    -> IO CabalSourceDir
unpackCabal cabalVer tmpdir variant = do
  let cabal = "Cabal-" ++ showVersion cabalVer
      dir = tmpdir </> cabal
      variant_opts = case variant of Pristine -> [ "--pristine" ]; _ -> []
      args = [ "get", cabal ] ++ variant_opts
  callProcessStderr (Just tmpdir) (cabalProgram ?progs) args
  return $ CabalSourceDir dir

unpackCabalHEAD :: Env => FilePath -> IO (CabalSourceDir, CommitId)
unpackCabalHEAD tmpdir = do
  let dir = tmpdir </> "cabal-head.git"
      url = "https://github.com/haskell/cabal.git"
  ExitSuccess <- rawSystem "git" [ "clone", "--depth=1", url, dir]
  commit <-
      withDirectory_ dir $ trim <$> readProcess' "git" ["rev-parse", "HEAD"] ""
  return (CabalSourceDir $ dir </> "Cabal", CommitId commit)
 where
   withDirectory_ :: FilePath -> IO a -> IO a
   withDirectory_ dir action =
       bracket
         (liftIO getCurrentDirectory)
         (liftIO . setCurrentDirectory)
         (\_ -> liftIO (setCurrentDirectory dir) >> action)

findCabalFile :: FilePath -> IO FilePath
findCabalFile pkgdir = do
    [cfile] <- filter isCabalFile <$> getDirectoryContents pkgdir
    return cfile
  where
    isCabalFile :: FilePath -> Bool
    isCabalFile f = takeExtension' f == ".cabal"

    takeExtension' :: FilePath -> String
    takeExtension' p =
        if takeFileName p == takeExtension p
          then "" -- just ".cabal" is not a valid cabal file
          else takeExtension p

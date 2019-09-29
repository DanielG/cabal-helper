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
Description : Cabal library source unpacking
License     : Apache-2.0
-}

{-# LANGUAGE DeriveFunctor, ViewPatterns, OverloadedStrings, CPP #-}

module CabalHelper.Compiletime.Cabal where

import Data.Char
import Control.Exception
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Version
import System.Directory
import System.FilePath
import System.IO
import Text.Printf

import Distribution.Verbosity (Verbosity, silent, normal, verbose, deafening)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Process
import CabalHelper.Shared.Common (replace, parseVer, parseVerMay, parsePkgIdBS, panicIO)

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
  callProcessStderr (Just tmpdir) [] (cabalProgram ?progs) args
  return $ CabalSourceDir dir

unpackCabalHEAD :: Env => FilePath -> IO (CommitId, CabalSourceDir)
unpackCabalHEAD tmpdir = do
  let dir = tmpdir </> "cabal-head.git"
      url = "https://github.com/haskell/cabal.git"
  callProcessStderr (Just "/") [] "git" [ "clone", "--depth=1", url, dir]
  callProcessStderr (Just (dir </> "Cabal")) [] "cabal"
    [ "act-as-setup", "--", "sdist"
    , "--output-directory=" ++ tmpdir </> "Cabal" ]
  commit <- takeWhile isHexDigit <$>
    readCreateProcess (proc "git" ["rev-parse", "HEAD"]){ cwd = Just dir } ""
  ts <-
    readCreateProcess (proc "git" [ "show", "-s", "--format=%ct", "HEAD" ])
      { cwd = Just dir } ""
  let ut = posixSecondsToUTCTime $ fromInteger (read ts)
      (y,m,d) = toGregorian $ utctDay ut
      sec = round $ utctDayTime ut
      datecode =
        read $ show y ++ printf "%02d" m ++ printf "%02d" d ++ printf "%05d" sec
      sec :: Int; datecode :: Int
  let cabal_file = tmpdir </> "Cabal/Cabal.cabal"
  cf0 <- readFile cabal_file
  let Just cf1 = replaceVersionDecl (setVersion datecode) cf0
  writeFile (cabal_file<.>"tmp") cf1
  renameFile (cabal_file<.>"tmp") cabal_file
  return (CommitId commit, CabalSourceDir $ tmpdir </> "Cabal")
  where
    -- If the released version of cabal has 4 components but we use only three
    -- theirs will always be larger than this one here. That's not really
    -- critical though.
    setVersion i (versionBranch -> mj:mi:_:_:[]) =
        Just $ makeVersion $ mj:mi:[i]
    setVersion _ v =
        error $ "unpackCabalHEAD.setVersion: Wrong version format" ++ show v

-- | Replace the version declaration in a cabal file
replaceVersionDecl :: (Version -> Maybe Version) -> String -> Maybe String
replaceVersionDecl ver_fn cf = let
  isVersionDecl ([],t) = "version:" `isPrefixOf` t
  isVersionDecl (i,t) = "\n" `isSuffixOf` i && "version:" `isPrefixOf` t
  Just (before_ver,m) = find isVersionDecl $ splits cf
  Just (ver_decl,after_ver)
    = find (\s -> case s of (_i,'\n':x:_) -> not $ isSpace x; _ -> False)
    $ filter (\(_i,t) -> "\n" `isPrefixOf` t)
    $ splits m
  Just vers0 = dropWhile isSpace <$> stripPrefix "version:" ver_decl
  (vers1,rest) = span (\c -> isDigit c || c == '.') vers0
  Just verp | all isSpace rest = parseVerMay $ vers1 in do
  new_ver <- ver_fn verp
  return $ concat
    [ before_ver, "version: ", showVersion new_ver, after_ver ]
  where
    splits xs = inits xs `zip` tails xs

resolveCabalVersion :: Verbose => CabalVersion -> IO ResolvedCabalVersion
resolveCabalVersion (CabalVersion ver) = return $ CabalVersion ver
resolveCabalVersion (CabalHEAD ()) = do
  out <- readProcess' "git"
    [ "ls-remote", "https://github.com/haskell/cabal.git", "-h", "master" ] ""
  let commit = takeWhile isHexDigit out
  return $ CabalHEAD $ CommitId commit

findCabalFile :: FilePath -> IO (Maybe FilePath)
findCabalFile pkgdir = do
    cfiles <- filter isCabalFile <$> getDirectoryContents pkgdir
    case cfiles of
      [] -> return Nothing
      [cfile] -> return $ Just $ pkgdir </> cfile
      _ -> panicIO $ "Multiple cabal-files found in directory '"
             ++pkgdir++"': " ++ show cfiles
  where
    isCabalFile :: FilePath -> Bool
    isCabalFile f = takeExtension' f == ".cabal"

    takeExtension' :: FilePath -> String
    takeExtension' p =
        if takeFileName p == takeExtension p
          then "" -- just ".cabal" is not a valid cabal file
          else takeExtension p

complainIfNoCabalFile :: FilePath -> Maybe FilePath -> IO FilePath
complainIfNoCabalFile _ (Just cabal_file) = return cabal_file
complainIfNoCabalFile pkgdir Nothing =
  panicIO $ "No cabal file found in package-dir: '"++pkgdir++"'"

bultinCabalVersion :: Version
bultinCabalVersion = parseVer VERSION_Cabal

readSetupConfigHeader :: FilePath -> IO (Maybe UnitHeader)
readSetupConfigHeader file = bracket (openFile file ReadMode) hClose $ \h -> do
  parseSetupHeader <$> BS.hGetLine h

parseSetupHeader :: BS.ByteString -> Maybe UnitHeader
parseSetupHeader header = case BS8.words header of
  ["Saved", "package", "config", "for", pkgId ,
   "written", "by", setupId,
   "using", compId]
    -> UnitHeader
       <$> parsePkgIdBS pkgId
       <*> parsePkgIdBS setupId
       <*> parsePkgIdBS compId
  _ -> Nothing

getCabalVerbosity :: Verbose => Verbosity
getCabalVerbosity
  | ?verbose 2 = normal
  | ?verbose 3 = verbose
  | ?verbose 4 = deafening
  | otherwise = silent

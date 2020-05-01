-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2020  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-|
Module      : CabalHelper.Compiletime.Types.Cabal
License     : Apache-2.0
-}

{-# LANGUAGE DeriveFunctor #-}

module CabalHelper.Compiletime.Types.Cabal where

import Data.Version

-- | Cabal library version we're compiling the helper exe against.
data CabalVersion' a
    = CabalHEAD a
    | CabalVersion { cvVersion :: Version }
      deriving (Eq, Ord, Functor)

newtype CommitId = CommitId { unCommitId :: String }

type UnpackedCabalVersion = CabalVersion' (CommitId, CabalSourceDir)
type ResolvedCabalVersion = CabalVersion' CommitId
type CabalVersion = CabalVersion' ()

data UnpackCabalVariant = Pristine | LatestRevision
newtype CabalSourceDir = CabalSourceDir { unCabalSourceDir :: FilePath }


unpackedToResolvedCabalVersion :: UnpackedCabalVersion -> ResolvedCabalVersion
unpackedToResolvedCabalVersion (CabalHEAD (commit, _)) = CabalHEAD commit
unpackedToResolvedCabalVersion (CabalVersion ver) = CabalVersion ver

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

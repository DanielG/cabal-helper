-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2017-2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE CPP #-}
module CabalHelper.Compiletime.Compat.Version
    ( DataVersion
    , toDataVersion
    , fromDataVersion
    , Data.Version.showVersion
    , makeDataVersion
    ) where

import qualified Data.Version
import qualified Distribution.Version (Version)
#if MIN_VERSION_Cabal(2,0,0)
import qualified Distribution.Version  (versionNumbers, mkVersion)
#endif

type DataVersion = Data.Version.Version

toDataVersion :: Distribution.Version.Version -> Data.Version.Version
fromDataVersion :: Data.Version.Version -> Distribution.Version.Version
#if MIN_VERSION_Cabal(2,0,0)
toDataVersion v = Data.Version.Version (Distribution.Version.versionNumbers v) []
fromDataVersion (Data.Version.Version vs _) = Distribution.Version.mkVersion vs
#else
toDataVersion = id
fromDataVersion = id
#endif

makeDataVersion :: [Int] -> Data.Version.Version
#if MIN_VERSION_base(4,8,0)
makeDataVersion = Data.Version.makeVersion
#else
makeDataVersion xs = Data.Version.Version xs []
#endif

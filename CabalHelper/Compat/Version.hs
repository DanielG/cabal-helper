{-# LANGUAGE CPP #-}
module CabalHelper.Compat.Version
    ( DataVersion
    , toDataVersion
    , fromDataVersion
    , Data.Version.showVersion
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

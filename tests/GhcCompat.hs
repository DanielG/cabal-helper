{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
--  ^ gbracket below doesn't get exported by module GhcCompat for ghc<9

module GhcCompat
    ( module GhcCompat
    , module GHC
#if MIN_VERSION_ghc(9,0,0)
     -- >=9.0.0
    , module GHC.Settings.Config
    , module GHC.Utils.Outputable
    , module GHC.Utils.Exception
    , module GHC.Driver.Session
#else
    -- < 9.0.0
    , module Config
    , module Outputable
    , module DynFlags
#endif
    ) where

import GHC
#if MIN_VERSION_ghc(9,0,0)
-- >=9.0.0
import GHC.Settings.Config
import GHC.Utils.Outputable
import GHC.Utils.Exception
import GHC.Driver.Session
#else
-- < 9.0.0
import Config
import Outputable
import DynFlags
#endif

#if MIN_VERSION_ghc(9,0,0)
gbracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
gbracket = bracket
#endif

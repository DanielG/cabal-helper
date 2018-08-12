{-# LANGUAGE CPP #-}
module CabalHelper.Compiletime.Compat.ProgramDb
    ( defaultProgramDb
    , programPath
    , lookupProgram
    , ghcProgram
    , ghcPkgProgram
    ) where

import Distribution.Simple.Program

#if !MIN_VERSION_Cabal(2,0,0)
defaultProgramDb = defaultProgramConfiguration
#endif

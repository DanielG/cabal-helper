-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2017  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE CPP #-}
module CabalHelper.Compiletime.Compat.Environment where

import qualified System.Environment
#ifndef mingw32_HOST_OS
import qualified System.Posix.Env (setEnv)
#endif

lookupEnv :: String -> IO (Maybe String)
lookupEnv var =
  do env <- System.Environment.getEnvironment
     return (lookup var env)

setEnv :: String -> String -> IO ()
#ifdef mingw32_HOST_OS
setEnv = System.Environment.setEnv
#else
setEnv k v = System.Posix.Env.setEnv k v True
#endif

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
Module      : CabalHelper.Compiletime.Log
Description : Logging utilities
License     : Apache-2.0
-}

module CabalHelper.Compiletime.Log where

import Control.Monad.IO.Class
import System.IO
import System.IO.Error

import CabalHelper.Compiletime.Types

logIOError :: Verbose => String -> IO (Maybe a) -> IO (Maybe a)
logIOError label a = do
  a `catchIOError` \ex -> do
      vLog $ label ++ ": " ++ show ex
      return Nothing

vLog :: (MonadIO m, Verbose) => String -> m ()
vLog msg
    | ?verbose 0 = liftIO $ hPutStrLn stderr msg
    | otherwise = return ()

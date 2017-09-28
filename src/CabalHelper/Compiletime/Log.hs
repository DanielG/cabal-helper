-- Copyright (C) 2017  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : CabalHelper.Compiletime.Log
Description : Basic logging facilities
License     : AGPL-3
-}

module CabalHelper.Compiletime.Log where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception as E
import Data.String
import System.IO
import Prelude

import CabalHelper.Compiletime.Types

vLog :: MonadIO m => Options -> String -> m ()
vLog Options { verbose = True } msg =
    liftIO $ hPutStrLn stderr msg
vLog _ _ = return ()

logIOError :: Options -> String -> IO (Maybe a) -> IO (Maybe a)
logIOError opts label a = do
  a `E.catch` \(ex :: IOError) -> do
      vLog opts $ label ++ ": " ++ show ex
      return Nothing

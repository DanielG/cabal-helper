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
Module      : CabalHelper.Compiletime.Log
Description : Logging utilities
License     : GPL-3
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

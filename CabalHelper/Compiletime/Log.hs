{-# LANGUAGE ScopedTypeVariables #-}

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

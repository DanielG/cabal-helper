module CabalHelper.Log where

import Control.Monad
import Control.Monad.IO.Class
import Data.String
import System.IO
import Prelude

import CabalHelper.Types

vLog :: MonadIO m => Options -> String -> m ()
vLog Options { verbose = True } msg =
    liftIO $ hPutStrLn stderr msg
vLog _ _ = return ()

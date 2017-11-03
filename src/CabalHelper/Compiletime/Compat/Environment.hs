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

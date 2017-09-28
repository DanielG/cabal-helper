module CabalHelper.Compiletime.Compat.Environment where

import System.Environment

lookupEnv :: String -> IO (Maybe String)
lookupEnv var = do env <- getEnvironment; return (lookup var env)

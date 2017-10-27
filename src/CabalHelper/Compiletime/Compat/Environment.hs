module CabalHelper.Compiletime.Compat.Environment where

import qualified System.Environment (getEnvironment)

lookupEnv :: String -> IO (Maybe String)
lookupEnv var =
  do env <- System.Environment.getEnvironment
     return (lookup var env)

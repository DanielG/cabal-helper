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
Module      : CabalHelper.Compiletime.Process
Description : System process utilities
License     : Apache-2.0
-}

module CabalHelper.Compiletime.Process
    ( module CabalHelper.Compiletime.Process
    , module System.Process
    ) where

import Control.Arrow (second)
import Data.Char
import Data.List
import qualified Data.Map.Strict as Map
import GHC.IO.Exception (IOErrorType(OtherError))
import System.IO
import System.IO.Error
import System.Environment
import System.Exit
import System.Process

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Log

readProcess' :: Verbose => FilePath -> [String] -> String -> IO String
readProcess' exe args inp =
  readProcessStderr Nothing [] exe args inp

readProcessStderr :: Verbose => Maybe FilePath -> [(String, EnvOverride)]
                  -> FilePath -> [String] -> String -> IO String
readProcessStderr mcwd env exe args inp = do
  logProcessCall mcwd env exe args
  env' <- execEnvOverrides env
  outp <- readCreateProcess (proc exe args)
    { cwd = mcwd
    , env = if env == [] then Nothing else Just env'
    } inp
  vLog $ unlines $ map ("=> "++) $ lines outp
  return outp

-- | Essentially 'System.Process.callProcess' but returns exit code, has
-- additional options and logging to stderr when verbosity is enabled.
callProcessStderr'
    :: Verbose => Maybe FilePath -> [(String, EnvOverride)]
    -> FilePath -> [String] -> IO ExitCode
callProcessStderr' mcwd env exe args = do
  logProcessCall mcwd env exe args
  env' <- execEnvOverrides env
  (_, _, _, h) <- createProcess (proc exe args)
    { std_out = UseHandle stderr
    , env = if env == [] then Nothing else Just env'
    , cwd = mcwd
    }
  waitForProcess h

logProcessCall :: Verbose => Maybe FilePath -> [(String, EnvOverride)]
               -> FilePath -> [String] -> IO ()
logProcessCall mcwd env exe args = do
  vLog $ intercalate " " $ cd ++ env_args ++ map formatProcessArg (exe:args)
  where
    env_args = map (\(k,v) -> k ++ "=" ++ show v) env
    cd = case mcwd of
      Nothing -> []; Just cwd -> [ "cd", formatProcessArg cwd++";" ]

execEnvOverride :: EnvOverride -> String -> String
execEnvOverride (EnvPrepend x) y = x ++ y
execEnvOverride (EnvAppend  y) x = x ++ y
execEnvOverride (EnvReplace x) _ = x

execEnvOverrides :: [(String, EnvOverride)] -> IO [(String, String)]
execEnvOverrides overrides = do
  envs <- getEnvironment
  return $ do
    (k,v) <- envs
    case Map.lookup k overrides_map of
      Just os -> return (k, foldr execEnvOverride v os)
      Nothing -> return (k, v)
  where
    overrides_map = Map.fromListWith (++) $ map (second (:[])) overrides

-- | Essentially 'System.Process.callProcess' but with additional options
-- and logging to stderr when verbosity is enabled.
callProcessStderr :: Verbose => Maybe FilePath -> [(String, EnvOverride)]
                  -> FilePath -> [String] -> IO ()
callProcessStderr mwd env exe args = do
  rv <- callProcessStderr' mwd env exe args
  case rv of
    ExitSuccess -> return ()
    ExitFailure v -> processFailedException "callProcessStderr" exe args v

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fn exe args rv =
    ioError $ mkIOError OtherError msg Nothing Nothing
  where
    msg = concat [ fn, ": ", exe, " "
                 , intercalate " " (map formatProcessArg args)
                 , " (exit " ++ show rv ++ ")"
                 ]

formatProcessArg :: String -> String
formatProcessArg xs
    | any isSpace xs = "'"++ xs ++"'"
    | otherwise      = xs

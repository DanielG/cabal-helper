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
Module      : CabalHelper.Compiletime.Process
Description : System process utilities
License     : GPL-3
-}

module CabalHelper.Compiletime.Process
    ( module CabalHelper.Compiletime.Process
    , module System.Process
    ) where

import Data.Char
import Data.List
import GHC.IO.Exception (IOErrorType(OtherError))
import System.IO
import System.IO.Error
import System.Exit
import System.Process

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.Log

readProcess' :: Verbose => FilePath -> [String] -> String -> IO String
readProcess' exe args inp = do
  vLog $ intercalate " " $ map formatProcessArg (exe:args)
  outp <- readProcess exe args inp
  vLog $ unlines $ map ("=> "++) $ lines outp
  return outp


callProcessStderr'
    :: Verbose => Maybe FilePath -> FilePath -> [String] -> IO ExitCode
callProcessStderr' mwd exe args = do
  let cd = case mwd of
             Nothing -> []; Just wd -> [ "cd", formatProcessArg wd++";" ]
  vLog $ intercalate " " $ cd ++ map formatProcessArg (exe:args)
  (_, _, _, h) <- createProcess (proc exe args) { std_out = UseHandle stderr
                                                , cwd = mwd }
  waitForProcess h

callProcessStderr :: Verbose => Maybe FilePath -> FilePath -> [String] -> IO ()
callProcessStderr mwd exe args = do
  rv <- callProcessStderr' mwd exe args
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

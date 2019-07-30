{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Distribution.Helper
import Data.Foldable
    ( toList )
import System.Process
    ( system )
import System.Environment
    ( getArgs )
import System.Exit
    ( ExitCode(ExitSuccess) )
import System.IO
    ( hPutStrLn, stderr )
import System.Console.GetOpt
    ( OptDescr(Option), ArgDescr(NoArg), ArgOrder(RequireOrder), getOpt
    , usageInfo )

main :: IO ()
main = do
  args <- getArgs
  actions <- parseOpts args
  sequence_ actions

-- | Run shell command and
systemV :: String -> IO ()
systemV shell_cmd = do
  hPutStrLn stderr $ "$ " ++ shell_cmd
  ExitSuccess <- system shell_cmd
  return ()

options :: [OptDescr (IO ())]
options =
 [ Option [] ["cabal"]          (NoArg doCabalV2)     ""
 , Option [] ["cabal-old-v1"]   (NoArg doCabalV1)     ""
 , Option [] ["stack"]          (NoArg doCabalV2)     ""
 ]

parseOpts :: [String] -> IO [IO ()]
parseOpts argv =
   case getOpt RequireOrder options argv of
      (o, [], []  ) ->
          return o
      (_, _, errs) ->
          ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: examples (--cabal|--cabal-old-v1|--stack)..."

doCabalV2 :: IO ()
doCabalV2 = do
  _ <- systemV "cabal new-build --builddir=dist-newstyle"
  qe <- mkQueryEnv (ProjLocV2Dir ".") (DistDirCabal SCV2 "dist-newstyle/")
  printUnitInfos qe

doCabalV1 :: IO ()
doCabalV1 = return ()

doStack :: IO ()
doStack = return ()

printUnitInfos :: QueryEnv pt -> IO ()
printUnitInfos qe = do
  components :: [ChComponentInfo]
      <- concat <$> runQuery (allUnits (toList . uiComponents)) qe
  print components

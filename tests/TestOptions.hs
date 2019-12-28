module TestOptions
    ( ModProgs
    , testOpts
    ) where

import System.Console.GetOpt as GetOpt

import CabalHelper.Compiletime.Types

type ModProgs = (Programs -> Programs)

options :: [OptDescr ModProgs]
options =
    [ GetOpt.Option [] ["with-cabal"]
        (ReqArg (\arg -> \p -> p { cabalProgram = arg }) "PROG")
        "name or path of 'cabal' executable"
    , GetOpt.Option [] ["with-stack"]
        (ReqArg (\arg -> \p -> p { stackProgram = arg }) "PROG")
        "name or path of 'stack' executable"
    , GetOpt.Option [] ["with-ghc"]
        (ReqArg (\arg -> \cp -> cp { ghcProgram = arg }) "PROG")
        "name or path of 'ghc' executable"
    , GetOpt.Option [] ["with-ghc-pkg"]
        (ReqArg (\arg -> \cp -> cp { ghcPkgProgram = arg }) "PROG")
        "name or path of 'ghc-pkg' executable"
    ]

testOpts :: [String] -> IO (ModProgs, [String])
testOpts args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (flip (.)) id o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ghc-session [OPTION..] [TEST_SPEC..]"

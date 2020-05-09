{-| This test checks if 'guessCompProgramPaths'\'s behaviour makes sense
-}

{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

import Control.Monad
import Data.List
import Distribution.Simple.Utils (dropExeExtension)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Temp
import System.Info
import Text.Show.Pretty

import CabalHelper.Compiletime.Types
import CabalHelper.Compiletime.CompPrograms
import Symlink (createSymbolicLink)

main :: IO ()
main = do
  -- In windows, program name ends with .exe
  prog_name <- dropExeExtension <$> getProgName
  args <- getArgs
  case prog_name of
    "programs-test"
      | "ghc":ver:rest     <- args  -> ghc ver rest
      | "ghc-pkg":ver:rest <- args  -> ghc_pkg ver rest
      | "haddock":ver:rest <- args  -> haddock ver rest
      | otherwise -> do_test
    _
      | Just ver <- stripPrefix "ghc-pkg-" prog_name  -> ghc_pkg ver args
      | Just ver <- stripPrefix "ghc-" prog_name      -> ghc ver args
      | Just ver <- stripPrefix "haddock-" prog_name  -> haddock ver args
  where
    ghc _ver ["--info"] = putStrLn "[]" -- seems we can get away with that :)
    ghc  ver ["--numeric-version"] = putStrLn ver
    ghc _ver ["--supported-languages"] = return ()

    ghc_pkg ver ["--version"] =
      putStrLn $ "GHC package manager version " ++ ver

    haddock _ver ["--version"] =
      putStrLn $ -- cabal isn't very picky about haddock versions so we just
                 -- hardocde it here
        "Haddock version 2.20.0, (c) Simon Marlow 2006" ++
        "Ported to use the GHC API by David Waern 2006-2008"

do_test :: IO ()
do_test = do
  prog <- canonicalizePath =<< getExecutablePath

  withSystemTempDirectory "c-h-programs-test" $ \tmpdir -> do

  forM_ ["8.6.5", "8.4.4"] $ \ver -> do

  let ghc = tmpdir </> "ghc-" ++ ver <.> exeExtension
  let ghc_pkg = tmpdir </> "ghc-pkg-" ++ ver <.> exeExtension
  let haddock = tmpdir </> "haddock-" ++ ver <.> exeExtension
  let progs = defaultPrograms { ghcProgram = ghc }

  let link = case System.Info.os of
              "mingw32" -> copyFile
              _         -> createSymbolicLink

  link prog ghc
  link prog ghc_pkg
  link prog haddock

  let ?verbose = (==4)

  progs' <- guessCompProgramPaths progs

  pPrint (ghc, ghc_pkg, haddock) -- expected
  pPrint progs' -- actual

  when (not $ and [ ghcPkgProgram progs'  == ghc_pkg
                  , haddockProgram progs' == haddock
                  ])
    exitFailure

  putStr "\n\n"

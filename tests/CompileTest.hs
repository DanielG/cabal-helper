{-# LANGUAGE ScopedTypeVariables, GADTs, ImplicitParams, OverloadedStrings #-}

{-| This test tries to compile the Helper against every supported version of the
  Cabal library. Since we compile the Helper at runtime, on the user's machine,
  it is very important to make sure this will not fail to compile.

  This test only covers using v2-build to install the requested Cabal library
  version because it has the best build product caching (keeps CI times
  down). We could also use stack since it has a global package cache but we
  don't support that because stack always comes with the right Cabal library
  version available for a given resolver anyways.
-}

import System.Environment (getArgs)
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import System.IO.Temp
import Data.List
import Data.Maybe
import Data.Version
import Data.Functor
import Data.Function
import Distribution.Version (VersionRange, withinRange)
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Maybe
import Prelude

import CabalHelper.Compiletime.Compat.Environment
import CabalHelper.Compiletime.Compat.Version
import CabalHelper.Compiletime.Compat.Parsec
import CabalHelper.Compiletime.Cabal
import CabalHelper.Compiletime.Compile
import CabalHelper.Compiletime.Program.GHC
import CabalHelper.Compiletime.Types
import CabalHelper.Shared.Common

import TestOptions

withinRange'CH :: Version -> VersionRange -> Bool
withinRange'CH v r =
    withinRange (fromDataVersion v) r

setupHOME :: IO ()
setupHOME = do
  mhome <- lookupEnv "HOME"
  case mhome of
    Just home -> do
      exists <- doesDirectoryExist home
      when (not exists) createHOME
    Nothing -> createHOME

createHOME :: IO ()
createHOME = do
  tmp <- fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  let home = tmp </> "compile-test-home"
  _ <- rawSystem "rm" ["-r", home]
  createDirectory home
  setEnv "HOME" home

main :: IO ()
main = do
  (modProgs, args) <- testOpts =<< getArgs

  let ?progs = modProgs defaultPrograms
  let ?opts = defaultCompileOptions { oVerbose = True }
  let ?verbose = \level -> case level of 1 -> True; _ -> False

  case args of
    "list-versions":[] -> do
        mapM_ print =<< relevantCabalVersions =<< ghcVersion
    "list-versions":ghc_ver_str:[] ->
        mapM_ print =<< relevantCabalVersions (GhcVersion (parseVer ghc_ver_str))
    _ ->
        test args

test :: Env => [String] -> IO ()
test args = do
  let action
       | null args = testRelevantCabalVersions
       | otherwise = testCabalVersions $ map parseVer' args

  setupHOME

  action

parseVer' :: String -> CabalVersion
parseVer' "HEAD" = CabalHEAD ()
parseVer' v      = CabalVersion $ parseVer v

relevantCabalVersions :: GhcVersion -> IO [Version]
relevantCabalVersions g = map snd . filter fst <$> allCabalVersions g

allCabalVersions :: GhcVersion -> IO [(Bool,Version)]
allCabalVersions (GhcVersion ghc_ver) = do
  cabal_versions
      <- map parseVer . lines <$> readFile "tests/cabal-versions"
  let
    constraint :: VersionRange
    Just constraint =
        fmap snd $
        find (and . (zipWith (==) `on` versionBranch) ghc_ver . fst) $
        constraint_table

    constraint_table :: [(Version, VersionRange)]
    constraint_table =
        map (parseVer *** (absorbParsecFailure "constraint_table" . eitherParsec)) $
            -- , ("7.8"  , ">= 1.18    && < 2")
            [ ("7.10" , ">= 1.22.2  && < 2")
            , ("8.0.1", ">= 1.24          ")
            , ("8.0.2", ">= 1.24.2        ")
            , ("8.2",   ">= 1.24.2.0      ")
            , ("8.4",   ">= 2.0.0.2       ")
            , ("8.6",   ">= 2.0.0.2       ")
            , ("8.8",   ">= 3.0.0.0       ")
            ]
  return $ reverse $ map (flip withinRange'CH constraint &&& id) cabal_versions


testRelevantCabalVersions :: Env => IO ()
testRelevantCabalVersions = do
  ghc_ver <- ghcVersion
  relevant_cabal_versions <- relevantCabalVersions ghc_ver
  testCabalVersions $ map CabalVersion relevant_cabal_versions ++ [CabalHEAD ()]

testCabalVersions :: Env => [CabalVersion] -> IO ()
testCabalVersions versions = do
--  ghcVer <- ghcVersion
  rvs <- forM versions $ \cv -> do
    withSystemTempDirectory "cabal-helper.proj-local-tmp" $ \tmpdir -> do

    let sver = showCabalVersion cv
    hPutStrLn stderr $ "\n\n\n\n\n\n====== Compiling with Cabal-" ++ sver

    let che0 = \icv db -> CompHelperEnv
          { cheCabalVer = icv
          , chePkgDb = db
          , cheProjDir = tmpdir
          , chePlanJson = Nothing
          , cheDistV2 = Just $ tmpdir </> "dist-newstyle"
          , cheProjLocalCacheDir =
              tmpdir </> "dist-newstyle" </> "cache"
          }

    che <- case cv of
      CabalHEAD () -> do
        rcv <- resolveCabalVersion cv
        db <- getPrivateCabalPkgDb rcv
        mcabalVersions <- runMaybeT $ listCabalVersions (Just db)
        case mcabalVersions of
          Just [hdver] ->
            return $ che0 (CabalVersion hdver) (Just db)
          _ ->
            return $ che0 (CabalHEAD ()) Nothing
      (CabalVersion ver) ->
        return $ che0 (CabalVersion ver) Nothing

    compileHelper che

  let printStatus (cv, rv) = putStrLn $ "- Cabal "++ver++" "++status
        where  ver = showCabalVersion cv
               status = case rv of
                         Right _ ->
                             "succeeded"
                         Left rvc ->
                             "failed (exit code "++show rvc++")"

  let drvs = versions `zip` rvs

  mapM_ printStatus drvs
  if any isLeft' $ map snd $ filter ((/=(CabalHEAD ())) . fst) drvs
     then exitFailure
     else exitSuccess

 where
   isLeft' (Left _) = True
   isLeft' (Right _) = False

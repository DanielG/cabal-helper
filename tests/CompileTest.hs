{-# LANGUAGE ScopedTypeVariables, GADTs, ImplicitParams #-}

{-| This test tries to compile the Helper against every supported version of the
  Cabal library. Since we compile the Helper at runtime, on the user's machine,
  it is very important to make sure this will not fail.
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
  let ?progs = defaultPrograms
  let ?cprogs = defaultCompPrograms
  let ?opts = defaultCompileOptions { oVerbose = True }
  let ?verbose = True

  args <- getArgs
  case args of
    "list-versions":[] -> do
        mapM_ print =<< (allCabalVersions <$> ghcVersion)
    "list-versions":ghc_ver_str:[] ->
        mapM_ print $ allCabalVersions (GhcVersion (parseVer ghc_ver_str))
    _ ->
        test args

test :: Env => [String] -> IO ()
test args = do
  let action
       | null args = testAllCabalVersions
       | otherwise = testCabalVersions $ map parseVer' args

  setupHOME

  action

parseVer' :: String -> CabalVersion
parseVer' "HEAD" = CabalHEAD ()
parseVer' v      = CabalVersion $ parseVer v

allCabalVersions :: GhcVersion -> [Version]
allCabalVersions (GhcVersion ghc_ver) = let
    cabal_versions :: [Version]
    cabal_versions = map parseVer
         -- , "1.18.0"
         -- , "1.18.1"
         -- , "1.18.1.1"
         -- , "1.18.1.2"
         -- , "1.18.1.3"
         -- , "1.18.1.4"
         -- , "1.18.1.5"
         -- , "1.18.1.6"
         -- , "1.18.1.7"
         -- , "1.20.0.0"
         -- , "1.20.0.1"
         -- , "1.20.0.2"
         -- , "1.20.0.3"
         -- , "1.20.0.4"
         -- , "1.22.0.0"
         -- , "1.22.1.0"
         -- , "1.22.1.1"
         [ "1.22.2.0"
         , "1.22.3.0"
         , "1.22.4.0"
         , "1.22.5.0"
         , "1.22.6.0"
         , "1.22.7.0"
         , "1.22.8.0"
         , "1.24.0.0"
         -- , "1.24.1.0" -- deprecated
         , "1.24.2.0"
         , "2.0.0.2"
         , "2.0.1.0"
         , "2.0.1.1"
         , "2.2.0.0"
         , "2.2.0.1"
         , "2.4.0.0"
         , "2.4.0.1"
         , "2.4.1.0"
         ]

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
            ]
  in
    reverse $ filter (flip withinRange'CH constraint) cabal_versions


testAllCabalVersions :: Env => IO ()
testAllCabalVersions = do
  ghc_ver <- ghcVersion
  let relevant_cabal_versions = allCabalVersions ghc_ver
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

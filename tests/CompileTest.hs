import System.Posix.Env (setEnv)
import System.Process
import System.Exit
import System.IO
import Data.List
import Data.Maybe
import Data.Version
import Data.Functor
import Data.Function
import qualified Distribution.Compat.ReadP as Dist
import Distribution.Version hiding (Version, showVersion)
import Distribution.Text
import Control.Exception as E
import Control.Arrow
import Control.Monad
import Prelude

import CabalHelper.Compiletime.Compat.Environment
import CabalHelper.Compiletime.Compat.Version
import CabalHelper.Compiletime.Compile
import CabalHelper.Compiletime.Types
import CabalHelper.Shared.Common

runReadP'Dist :: Dist.ReadP t t -> String -> t
runReadP'Dist p i = case filter ((=="") . snd) $ Dist.readP_to_S p i of
                 (a,""):[] -> a
                 _ -> error $ "Error parsing: " ++ show i

withinRange'CH :: Either HEAD Version -> VersionRange -> Bool
withinRange'CH v r =
    withinRange (fromDataVersion v') r
  where
    v' = either (const $ parseVer "1000000000") id v

main :: IO ()
main = do
  flip (setEnv "HOME") True =<< fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  _ <- rawSystem "cabal" ["update"]

  let parseVer' "HEAD" = Left HEAD
      parseVer' v      = Right $ parseVer v

  let cabal_versions :: [Either HEAD Version]
      cabal_versions = map parseVer'
           -- "1.14.0" -- not supported at runtime
           [ "1.16.0"
           , "1.16.0.1"
           , "1.16.0.2"
           , "1.16.0.3"
           , "1.18.0"
           , "1.18.1"
           , "1.18.1.1"
           , "1.18.1.2"
           , "1.18.1.3"
           , "1.18.1.4"
           , "1.18.1.5"
           , "1.18.1.6"
           , "1.18.1.7"
           , "1.20.0.0"
           , "1.20.0.1"
           , "1.20.0.2"
           , "1.20.0.3"
           , "1.20.0.4"
           , "1.22.0.0"
           , "1.22.1.0"
           , "1.22.1.1"
           , "1.22.2.0"
           , "1.22.3.0"
           , "1.22.4.0"
           , "1.22.5.0"
           , "1.22.6.0"
           , "1.22.7.0"
           , "1.22.8.0"
           , "1.24.0.0"
           , "1.24.1.0"
           , "1.24.2.0"
           , "2.0.0.2"
           , "HEAD"
           ]

  ghc_ver <- ghcVersion defaultOptions

  let constraint :: VersionRange
      Just (_, constraint) =
          find (and . (zipWith (==) `on` versionBranch) ghc_ver . fst) $
          map (parseVer *** runReadP'Dist parse) $
              [ ("7.4"  , ">= 1.14    && < 2")
              , ("7.6"  , ">= 1.16    && < 2")
              , ("7.8"  , ">= 1.18    && < 2")
              , ("7.10" , ">= 1.22.2  && < 2")
              , ("8.0.1", ">= 1.24          ")
              , ("8.0.2", ">= 1.24.2        ")
              , ("8.2.1", ">= 1.24.2        ")
              ]

      relevant_cabal_versions =
          reverse $ filter (flip withinRange'CH constraint) cabal_versions

  rvs <- forM relevant_cabal_versions $ \ver -> do
           let sver = either show showVersion ver
           hPutStrLn stderr $ "\n\n\n\n\n\n====== Compiling with Cabal-" ++ sver
           compilePrivatePkgDb ver

  let printStatus (cv, rv) = putStrLn $ "- Cabal "++show cv++" "++status
        where status = case rv of
                         Right _ ->
                             "suceeded"
                         Left rvc ->
                             "failed (exit code "++show rvc++")"

  let drvs = relevant_cabal_versions `zip` rvs

  mapM_ printStatus (relevant_cabal_versions `zip` rvs)
  if any isLeft' $ map snd $ filter ((/=Left HEAD) . fst) drvs
     then exitFailure
     else exitSuccess

 where
   isLeft' (Left _) = True
   isLeft' (Right _) = False

data HEAD = HEAD deriving (Eq, Show)

compilePrivatePkgDb :: Either HEAD Version -> IO (Either ExitCode FilePath)
compilePrivatePkgDb (Left HEAD) = do
    res <- (Right <$> installCabalHEAD defaultOptions { verbose = True })
             `E.catch` \(SomeException ex) -> return $ Left $
                 "ERROR: Installing cabal HEAD failed: " ++ show ex
    case res of
      Left err -> do
          hPutStrLn stderr err
          return $ Left $ ExitFailure 1
      Right (db, commit) ->
          compileWithPkg (Just db) (Left commit)
compilePrivatePkgDb (Right cabalVer) = do
    db <- installCabal defaultOptions { verbose = True } cabalVer `E.catch`
        \(SomeException _) -> do
            errorInstallCabal cabalVer "/does-not-exist"
    compileWithPkg (Just db) (Right cabalVer)

compileWithPkg :: Maybe FilePath
               -> Either String Version
               -> IO (Either ExitCode FilePath)
compileWithPkg mdb ver =
    compile "/does-not-exist" defaultOptions { verbose = True } $
      Compile Nothing mdb ver [cabalPkgId ver]

cabalPkgId :: Either String Version -> String
cabalPkgId (Left _commitid) = "Cabal"
cabalPkgId (Right v) = "Cabal-" ++ showVersion v

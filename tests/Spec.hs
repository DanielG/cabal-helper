import Distribution.Helper
import System.Environment.Extra (lookupEnv)
import System.Posix.Env (setEnv)
import System.Process
import System.Exit
import Data.Maybe
import Data.Version
import Data.Functor
import Control.Exception as E
import Control.Arrow
import Prelude

import CabalHelper.Common
import CabalHelper.Compile
import CabalHelper.Types


main :: IO ()
main = do
  flip (setEnv "HOME") True =<< fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  _ <- rawSystem "cabal" ["update"]

  writeAutogenFiles' $ defaultQueryEnv "." "./dist"

  let parseVer' "HEAD" = Left HEAD
      parseVer' v = Right $ parseVer v

  let vers :: [(Version, [Either HEAD Version])]
      vers = map (parseVer *** map parseVer') [
               ("7.4", [ -- "1.14.0" -- not supported at runtime
                       ]),

               ("7.6", [ "1.16.0"
                       , "1.16.0.1"
                       , "1.16.0.2"
                       , "1.16.0.3"
                       ]),

               ("7.8", [
                         "1.18.0"
                       , "1.18.1"
                       , "1.18.1.1"
                       , "1.18.1.2"
                       , "1.18.1.3"
                       , "1.18.1.4"
                       , "1.18.1.5"
                       , "1.18.1.6"

                       , "1.20.0.0"
                       , "1.20.0.1"
                       , "1.20.0.2"
                       , "1.20.0.3"
                       , "1.22.0.0"
                       , "1.22.1.0"
                       , "1.22.1.1"
                       ]),

               ("7.10", [
                         "1.22.2.0"
                       , "1.22.3.0"
                       , "1.22.4.0"
                       , "1.22.5.0"
                       , "1.22.6.0"
                       , "HEAD"
                       ]),
               ("8.0", [
                       ])
             ]

  ghcVer <- majorVer <$> ghcVersion defaultOptions

  let cabalVers = reverse $ concat $ map snd $ dropWhile ((<ghcVer) . fst) vers

  rvs <- mapM compilePrivatePkgDb cabalVers

  if any isLeft' rvs
     then print rvs >> exitFailure
     else exitSuccess
 where
   isLeft' (Left _) = True
   isLeft' (Right _) = False

data HEAD = HEAD deriving (Show)

compilePrivatePkgDb :: Either HEAD Version -> IO (Either ExitCode FilePath)
compilePrivatePkgDb (Left HEAD) = do
    _ <- rawSystem "rm" [ "-r", "/tmp/.ghc-mod" ]
    (db, commit) <- installCabalHEAD defaultOptions { verbose = True } `E.catch`
        \(SomeException ex) -> error $ "Installing cabal HEAD failed: " ++ show ex
    compileWithPkg "." (Just db) (Left commit)
compilePrivatePkgDb (Right cabalVer) = do
    _ <- rawSystem "rm" [ "-r", "/tmp/.ghc-mod" ]
    db <- installCabal defaultOptions { verbose = True } cabalVer `E.catch`
        \(SomeException _) -> errorInstallCabal cabalVer "dist"
    compileWithPkg "." (Just db) (Right cabalVer)

compileWithPkg :: FilePath
               -> Maybe FilePath
               -> Either String Version
               -> IO (Either ExitCode FilePath)
compileWithPkg chdir mdb ver =
    compile "dist" defaultOptions { verbose = True } $
      Compile chdir Nothing mdb ver [cabalPkgId ver]

cabalPkgId :: Either String Version -> String
cabalPkgId (Left _commitid) = "Cabal"
cabalPkgId (Right v) = "Cabal-" ++ showVersion v

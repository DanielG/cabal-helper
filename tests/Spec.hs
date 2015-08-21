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

import CabalHelper.Common
import CabalHelper.Compile
import CabalHelper.Types

main :: IO ()
main = do
  flip (setEnv "HOME") True =<< fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  writeAutogenFiles readProcess "." "./dist"

  _ <- system "cabal update"

  let vers :: [(Version, [Version])]
      vers = map (parseVer *** map parseVer) [
               ("7.4", [ "1.14.0"
                       ]),

               ("7.6", [ "1.16.0"
                       , "1.16.0.1"
                       , "1.16.0.2"
                       , "1.16.0.3"
                       ]),

               ("7.8", [
--                         "1.18.0"
--                       , "1.18.1"
                         "1.18.1.1"
--                       , "1.18.1.2"
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
                       ])
             ]

  ghcVer <- ghcVersion defaultOptions

  let cabalVers = concat $ map snd $ dropWhile ((<ghcVer) . fst)  vers

  rvs <- mapM compilePrivatePkgDb cabalVers

  if any isLeft' rvs
     then exitFailure
     else exitSuccess
 where
   isLeft' (Left _) = True
   isLeft' (Right _) = False

compilePrivatePkgDb :: Version -> IO (Either ExitCode FilePath)
compilePrivatePkgDb cabalVer = do
    db <- installCabal defaultOptions cabalVer `E.catch`
          \(SomeException _) -> errorInstallCabal cabalVer "dist"
    compileWithPkg "." (Just db) cabalVer

compileWithPkg :: FilePath -> Maybe FilePath -> Version -> IO (Either ExitCode FilePath)
compileWithPkg chdir mdb ver =
    compile "dist" defaultOptions $ Compile chdir Nothing mdb ver [cabalPkgId ver]

cabalPkgId :: Version -> String
cabalPkgId v = "Cabal-" ++ showVersion v

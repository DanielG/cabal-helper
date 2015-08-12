import Distribution.Helper
import System.Environment.Extra (lookupEnv)
import System.Posix.Env (setEnv)
import System.Process
import Data.Maybe
import Data.Functor

main :: IO ()
main = do
  flip (setEnv "HOME") True =<< fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  writeAutogenFiles readProcess "." "./dist"

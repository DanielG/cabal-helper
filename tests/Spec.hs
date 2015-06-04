import Distribution.Helper
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  setEnv "HOME" =<< fromMaybe "/tmp" <$> lookupEnv "TMPDIR"
  writeAutogenFiles "./dist"

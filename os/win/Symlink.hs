module Symlink (createSymbolicLink) where
import System.Win32.SymbolicLink (createSymbolicLinkFile)
createSymbolicLink = createSymbolicLinkFile

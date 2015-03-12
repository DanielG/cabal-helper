-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module CabalHelper.Data where

import Control.Monad
import Data.Functor
import Language.Haskell.TH
import System.FilePath
import System.Directory
import System.IO.Temp

withHelperSources :: (FilePath -> IO a) -> IO a
withHelperSources action = withSystemTempDirectory "caba-helper" $ \dir -> do
    let chdir = dir </> "CabalHelper"
    createDirectory chdir
    forM_ sourceFiles $ \(fn, src) -> writeFile (chdir </> fn) src
    action dir

sourceFiles :: [(FilePath, String)]
sourceFiles =
  [ ("Main.hs",   $(LitE . StringL <$> runIO (readFile "CabalHelper/Main.hs")))
  , ("Common.hs", $(LitE . StringL <$> runIO (readFile "CabalHelper/Common.hs")))
  , ("Types.hs",  $(LitE . StringL <$> runIO (readFile "CabalHelper/Types.hs")))
  ]

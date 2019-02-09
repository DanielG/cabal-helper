-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}
module CabalHelper.Compiletime.Compat.Parsec
    ( absorbParsecFailure
    , eitherParsec
    ) where

#if MIN_VERSION_Cabal(2,5,0)
import Distribution.Parsec
#else
import qualified Distribution.Compat.ReadP as Dist
import Distribution.Text
#endif

absorbParsecFailure :: String -> Either String a -> a
absorbParsecFailure _ (Right x) = x
absorbParsecFailure ctx (Left err) =
    error $ "Error parsing in '"++ctx++"': " ++ err

#if !MIN_VERSION_Cabal(2,5,0)
eitherParsec :: Text t => String -> Either String t
eitherParsec i =
  case filter ((=="") . snd) $ Dist.readP_to_S parse i of
    (a,""):[] -> Right a
    _ -> Left $ show i
#endif

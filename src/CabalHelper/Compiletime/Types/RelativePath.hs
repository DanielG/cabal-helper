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

{-|
Module      : CabalHelper.Compiletime.Types.RelativePath
License     : GPL-3
-}

module CabalHelper.Compiletime.Types.RelativePath
    ( RelativePath
    , mkRelativePath
    , unRelativePath
    ) where

import System.FilePath

-- | A path guaranteed to be relative. The constructor is not exposed, use the
-- 'mkRelativePath' smart constructor.
newtype RelativePath = RelativePath { unRelativePath :: FilePath }
    deriving (Show)

-- | Smart constructor for 'RelativePath'. Checks if the given path is absolute
-- and throws 'UserError' if not.
mkRelativePath :: FilePath -> RelativePath
mkRelativePath dir
    | isAbsolute dir = RelativePath dir
    | otherwise = error "mkRelativePath: the path given was absolute!"

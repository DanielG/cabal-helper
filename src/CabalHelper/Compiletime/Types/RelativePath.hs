-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-|
Module      : CabalHelper.Compiletime.Types.RelativePath
License     : Apache-2.0
-}

module CabalHelper.Compiletime.Types.RelativePath
    ( RelativePath
    , mkRelativePath
    , unRelativePath
    ) where

import System.FilePath

-- | A path guaranteed to be relative and not escape the base path. The
-- constructor is not exposed, use the 'mkRelativePath' smart constructor.
newtype RelativePath = RelativePath { unRelativePath :: FilePath }
    deriving (Show)

-- | Smart constructor for 'RelativePath'. Checks if the given path
-- satisfies the constraints and throws 'UserError' if not.
mkRelativePath :: FilePath -> RelativePath
mkRelativePath dir
  | isAbsolute dir =
    error $ "mkRelativePath: the path given was absolute! got: " ++ dir
  | doesRelativePathEscapeCWD dir =
    error $ "mkRelativePath: the path given escapes the base dir! got: " ++ dir
  | otherwise =
    RelativePath dir

doesRelativePathEscapeCWD :: FilePath -> Bool
doesRelativePathEscapeCWD path =
    go [] $ splitDirectories $ normalise path
       -- normalise collapses '.' in path, this is very important or this
       -- check would be traivial to defeat. For example './../' would be
       -- able to escape.
  where
    go (_:xs) ("..":ys) = go xs ys
    go    []  ("..":__) = True
    go    xs  (y   :ys) = go (y:xs) ys
    go    _         []  = False

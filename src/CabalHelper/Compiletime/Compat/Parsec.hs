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

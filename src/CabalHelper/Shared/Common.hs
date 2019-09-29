-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-|
Module      : CabalHelper.Shared.Common
Description : Shared utility functions
License     : Apache-2.0
-}

{-# LANGUAGE CPP, DeriveDataTypeable, OverloadedStrings #-}
module CabalHelper.Shared.Common where

#ifdef MIN_VERSION_Cabal
#undef CH_MIN_VERSION_Cabal
#define CH_MIN_VERSION_Cabal MIN_VERSION_Cabal
#endif

import Distribution.PackageDescription
    ( GenericPackageDescription
    )
import Distribution.Verbosity
    ( Verbosity
    )

#if CH_MIN_VERSION_Cabal(2,2,0)
import qualified Distribution.PackageDescription.Parsec as P
#else
import qualified Distribution.PackageDescription.Parse as P
#endif

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Version
import Data.Typeable
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Environment
import System.IO
import qualified System.Info
import System.Exit
import System.Directory
import System.FilePath
import Text.ParserCombinators.ReadP
import Prelude

data Panic = Panic String deriving (Typeable)
instance Exception Panic
instance Show Panic where
    show (Panic msg) = "panic! " ++ msg

panic :: String -> a
panic msg = throw $ Panic msg

panicIO :: String -> IO a
panicIO msg = throwIO $ Panic msg

handlePanic :: IO a -> IO a
handlePanic action =
    action `E.catch` \(Panic msg) -> errMsg msg >> exitFailure

errMsg :: String -> IO ()
errMsg str = do
  prog <- getProgName
  hPutStrLn stderr $ prog ++ ": " ++ str

parsePkgId :: String -> Maybe (String, Version)
parsePkgId s =
    case span (/='-') (reverse s) of
      (vers, '-':pkg) -> Just (reverse pkg, parseVer (reverse vers))
      _ -> Nothing

parsePkgIdBS :: ByteString -> Maybe (ByteString, Version)
parsePkgIdBS bs =
    case BS8.span (/='-') (BS.reverse bs) of
      (vers, pkg') ->
          Just ( BS.reverse $ BS.tail pkg'
               , parseVer (BS8.unpack (BS.reverse vers)))

parseVer :: String -> Version
parseVer vers = runReadP parseVersion vers

parseVerMay :: String -> Maybe Version
parseVerMay vers = runReadPMay parseVersion vers

trim :: String -> String
trim = dropWhileEnd isSpace

majorVer :: Version -> Version
majorVer (Version b _) = Version (take 2 b) []

sameMajorVersionAs :: Version -> Version -> Bool
sameMajorVersionAs a b = majorVer a == majorVer b

runReadP :: ReadP t -> String -> t
runReadP p i =
  case runReadPMay p i of
    Just x -> x
    Nothing -> error $ "Error parsing version: " ++ show i

runReadPMay :: ReadP t -> String -> Maybe t
runReadPMay p i = case filter ((=="") . snd) $ readP_to_S p i of
                 (a,""):[] -> Just a
                 _ -> Nothing


appCacheDir :: IO FilePath
appCacheDir =
    (</> "cabal-helper") <$> getEnvDefault "XDG_CACHE_HOME" (homeRel cache)
 where
    -- for GHC 7.4
    lookupEnv' var = do env <- getEnvironment; return (lookup var env)
    getEnvDefault var def = lookupEnv' var >>= \m -> case m of Nothing -> def; Just x -> return x
    homeRel path = (</> path) <$> getHomeDirectory
    cache =
        case System.Info.os of
          "mingw32" -> windowsCache
          _         -> unixCache

    windowsCache = "Local Settings" </> "Cache"
    unixCache = ".cache"

replace :: String -> String -> String -> String
replace n r hs' = go "" hs'
 where
   go acc h
       | take (length n) h == n =
           reverse acc ++ r ++ drop (length n) h
   go acc (h:hs) = go (h:acc) hs
   go acc [] = reverse acc


readPackageDescription
    :: Verbosity
    -> FilePath
    -> IO GenericPackageDescription
#if CH_MIN_VERSION_Cabal(2,0,0)
readPackageDescription = P.readGenericPackageDescription
#else
readPackageDescription = P.readPackageDescription
#endif

mightExist :: FilePath -> IO (Maybe FilePath)
mightExist f = do
  exists <- doesFileExist f
  return $ if exists then (Just f) else (Nothing)

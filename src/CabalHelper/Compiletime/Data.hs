-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2017  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, CPP #-}

{-|
Module      : CabalHelper.Compiletime.Data
Description : Embeds source code for runtime component using TH
License     : Apache-2.0
-}

module CabalHelper.Compiletime.Data where

import Control.Monad
import Control.Monad.IO.Class
import Data.Digest.Pure.SHA
import Data.Functor
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile)
import System.Directory
import System.FilePath
import System.IO.Temp
import System.PosixCompat.Files
import System.PosixCompat.Time
import System.PosixCompat.Types
import Prelude

import CabalHelper.Compiletime.Compat.Environment

withSystemTempDirectoryEnv :: String -> (FilePath -> IO b) -> IO b
withSystemTempDirectoryEnv tpl f = do
  m <- liftIO $ lookupEnv "CABAL_HELPER_KEEP_SOURCEDIR"
  case m of
    Nothing -> withSystemTempDirectory tpl f
    Just _  -> do
           tmpdir <- getCanonicalTemporaryDirectory
           f =<< createTempDirectory tmpdir tpl

createHelperSources :: FilePath -> IO ()
createHelperSources dir = do
    let chdir = dir </> "CabalHelper"
    liftIO $ do
      createDirectoryIfMissing True $ chdir </> "Runtime"
      createDirectoryIfMissing True $ chdir </> "Shared"

    let modtime :: EpochTime
        modtime = fromIntegral $ (read :: String -> Integer)
          -- See https://reproducible-builds.org/specs/source-date-epoch/
          $(runIO $ do
             msde :: Maybe Integer
                  <- fmap read <$> lookupEnv "SOURCE_DATE_EPOCH"
             (current_time :: Integer) <- round . toRational <$> epochTime
             return $ LitE . StringL $ show $ maybe current_time id msde)

    liftIO $ forM_ sourceFiles $ \(fn, src) -> do
        let path = chdir </> fn
        BS.writeFile path $ UTF8.fromString src
        setFileTimes path modtime modtime

sourceHash :: String
sourceHash  = fst runtimeSources

sourceFiles :: [(FilePath, String)]
sourceFiles = snd runtimeSources

runtimeSources :: (String, [(FilePath, FilePath)])
runtimeSources = $(
  let files = map (\f -> (f, ("src/CabalHelper" </> f))) $ sort $
        [ ("Runtime/Main.hs")
        , ("Runtime/HelperMain.hs")
        , ("Runtime/Compat.hs")
        , ("Shared/Common.hs")
        , ("Shared/InterfaceTypes.hs")
        ]
  in do
    contents <- forM (map snd files) $ \lf -> do
      addDependentFile lf
      runIO (LBS.readFile lf)
    let hashes = map (bytestringDigest . sha256) contents
    let top_hash = showDigest $ sha256 $ LBS.concat hashes

    let exprWrapper =
#if MIN_VERSION_template_haskell(2,16,0)
          Just
#else
          id
#endif


    thfiles <- forM (map fst files `zip` contents) $ \(f, xs) -> do
      return $ TupE [exprWrapper (LitE (StringL f)), exprWrapper (LitE (StringL (LUTF8.toString xs)))]


    return $ TupE [exprWrapper (LitE (StringL top_hash)), exprWrapper (ListE thfiles)]

  )

-- - $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile

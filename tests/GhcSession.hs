{-# LANGUAGE TupleSections, ScopedTypeVariables, RecordWildCards, RankNTypes,
  DataKinds, ExistentialQuantification, PolyKinds, ViewPatterns,
  DeriveFunctor, MonoLocalBinds, GADTs, MultiWayIf #-}

{-| This test ensures we can get a GHC API session up and running in a variety of
  project environments.
-}

module Main where

import GHC
import GHC.Paths (libdir)
import Outputable
import DynFlags

import Control.Arrow (second)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Tuple
import Data.Version
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>), (<.>), makeRelative, takeDirectory)
import System.Directory
import System.IO
import System.IO.Temp
import System.Process (readProcess)
import Text.Printf (printf)
import Text.Show.Pretty

import Distribution.Helper

import CabalHelper.Shared.Common
import CabalHelper.Compiletime.Process

data TestConfig = TC
  { location        :: TestLocation
  , cabalLowerBound :: Version
  , ghcLowerBound   :: Version
  , projTypes       :: [ProjType]
  } deriving (Show)

data TestLocation
  = TN String
  | TF FilePath FilePath FilePath
    deriving (Show)

main :: IO ()
main = do
  args <- getArgs
--  topdir <- getCurrentDirectory

  ci_ver <- cabalInstallVersion
  c_ver <- cabalInstallBuiltinCabalVersion
  g_ver <- ghcVersion
  s_ver <- stackVersion
    `E.catch` \(_ :: IOError) -> return (makeVersion [0])

  putStrLn $ "cabal-install version: " ++ showVersion ci_ver
  putStrLn $ "Cabal version: " ++ showVersion c_ver
  putStrLn $ "GHC version: " ++ showVersion g_ver
  putStrLn $ "Stack version: " ++ showVersion s_ver

  let proj_impls :: [(ProjType, ProjSetup0)]
      proj_impls =
        [ (V2,    newBuildProjSetup)
        , (V1,    oldBuildProjSetup)
        , (Stack, stackProjSetup g_ver)
        ]

  tests <- return $ case args of
    xs@(_:_) -> flip map xs $ \loc ->
      let (topdir, ':':x0) = span (/=':') loc
          (projdir0, ':':x1) = span (/=':') x0
          (cabal_file0, ':':pt) = span (/=':') x1
          projdir = makeRelative topdir projdir0
          cabal_file = makeRelative topdir cabal_file0 in
      TC (TF topdir projdir cabal_file) (parseVer "0") (parseVer "0") [read pt]
    [] ->
      [ TC (TN "exelib")    (parseVer "1.10") (parseVer "0")   []
      , TC (TN "exeintlib") (parseVer "2.0")  (parseVer "0")   []
      , TC (TN "fliblib")   (parseVer "2.0")  (parseVer "0")   []
      , TC (TN "bkpregex")  (parseVer "2.0")  (parseVer "8.1") [V2, V1]
      , let multipkg_loc = TF "tests/multipkg/" "proj/" "proj/proj.cabal" in
        TC  multipkg_loc    (parseVer "1.10") (parseVer "0")   [V2, Stack]
      --            min Cabal lib ver -^    min GHC ver -^
      ]

  -- pPrint tests
  -- mapM_ (\(TC loc _ _ _) -> pPrint $ testLocPath loc) tests

  res :: [[Bool]] <- sequence $ do
    tc@TC {..} <- tests
    (pt, ps0 :: ProjSetup0) <- proj_impls
    guard (null projTypes || pt `elem` projTypes)

    let skip (SkipReason reason) = do
          hPutStrLn stderr $ intercalate " "
            [ "Skipping test"
            , psdHeading ps0
            , "'" ++ projdir_rel ++ "'"
            , "because"
            , reason
            ]
          where
            (_, projdir_rel, _) = testLocPath location

    case psdImpl ps0 of
      Left reason -> return $ skip reason >> return []
      Right eximpl -> do
        let ps1 = ps0 { psdImpl = eximpl }
        case checkAndRunTestConfig VerEnv{..} ps1 tc of
          Left reason -> return $ skip reason >> return []
          Right (Message msg, act) -> return $ hPutStrLn stderr msg >> act

  if any (==False) $ concat res
    then exitFailure
    else exitSuccess

data VerEnv = VerEnv
  { ci_ver :: Version
  , c_ver  :: Version
  , g_ver  :: Version
  , s_ver  :: Version
  }

data Message = Message String
data SkipReason = SkipReason String

testLocPath :: TestLocation -> (FilePath, FilePath, FilePath)
testLocPath (TN test_name) = (projdir, ".", cabal_file)
  where
    projdir :: FilePath
    projdir = "tests" </> test_name
    cabal_file :: FilePath
    cabal_file = test_name <.> "cabal"
testLocPath (TF topdir projdir cabal_file) =
    (topdir, projdir, cabal_file)

data Ex a = forall x. Ex (a x)

checkAndRunTestConfig
    :: VerEnv
    -> ProjSetup1
    -> TestConfig
    -> Either SkipReason (Message, IO [Bool])
checkAndRunTestConfig
  VerEnv { ci_ver, c_ver, g_ver, s_ver }
  ps1@(psdImpl -> Ex psdImpl2)
  (TC test_loc min_cabal_ver min_ghc_ver _proj_types)
  = let
  (topdir, projdir_rel, cabal_file) = testLocPath test_loc
  mreason
    | SStack <- psiProjType psdImpl2
    , s_ver < parseVer "1.9.4" =
      if| g_ver >= parseVer "8.2.2" ->
          error $ printf
            "stack-%s is too old, but GHC %s is recent enough to build it.\n\
            \The CI scripts should have installed it! See 25-deps.sh\n"
            (showVersion s_ver) (showVersion g_ver)
        | otherwise ->
          Just $ "stack-" ++ showVersion s_ver ++ " is too old"
    | (ci_ver < parseVer "1.24") =
      Just $ "cabal-install-" ++ showVersion ci_ver ++ " is too old"
    | c_ver < min_cabal_ver =
      Just $ "Cabal-" ++ showVersion c_ver
             ++ " < " ++ showVersion min_cabal_ver
    | g_ver < min_ghc_ver =
      Just $ "ghc-" ++ showVersion g_ver
             ++ " < " ++ showVersion min_ghc_ver
    | otherwise =
      Nothing
  in case mreason of
    Just reason -> do
      Left $ SkipReason reason
    Nothing -> do
      Right $ (,)
        (Message $ intercalate " "
          [ "\n\n\nRunning test"
          , psdHeading ps1
          , "'" ++ topdir ++ "'"
          ])
        (runTest ps1{ psdImpl = psdImpl2 } topdir projdir_rel cabal_file)

runTest :: ProjSetup2 pt -> FilePath -> FilePath -> FilePath -> IO [Bool]
runTest ps2@(psdImpl -> ProjSetupImpl{..}) topdir projdir cabal_file = do
  withSystemTempDirectory' "cabal-helper.ghc-session.test" $ \tmpdir -> do
    psiSdist topdir tmpdir
    psiConfigure (tmpdir </> projdir)
    test ps2 (tmpdir </> projdir) (tmpdir </> cabal_file)

runWithCwd :: FilePath -> String -> [String] -> IO ()
runWithCwd cwd x xs = do
  let ?verbose = True
  callProcessStderr (Just cwd) x xs

run :: String -> [String] -> IO ()
run x xs = do
  let ?verbose = True
  callProcessStderr Nothing x xs

test :: ProjSetup2 pt -> FilePath -> FilePath -> IO [Bool]
test (psdImpl -> ProjSetupImpl{..}) projdir cabal_file = do
    qe <- psiQEmod <$> mkQueryEnv
            (psiProjLoc (CabalFile cabal_file) projdir)
            (psiDistDir projdir)

    cs <- concat <$> runQuery (allUnits (Map.elems . uiComponents)) qe

    when (any ((==ProduceBuildOutput) . ciNeedsBuildOutput) cs) $
      psiBuild projdir

    let pkgdir = takeDirectory cabal_file
    forM cs $ \ChComponentInfo{..} -> do
        putStrLn $ "\n" ++ show ciComponentName
                        ++ ":::: " ++ show ciNeedsBuildOutput

        let opts' = "-Werror" : ciGhcOptions
        let sopts = intercalate " " $ map formatArg $ "ghc" : opts'
        putStrLn $ "\n" ++ show ciComponentName ++ ":\n" ++ "cd " ++ pkgdir ++ "\n" ++ sopts
        hFlush stdout
        compileModule pkgdir ciNeedsBuildOutput ciEntrypoints ciSourceDirs opts'
  where
    formatArg x
        | "-" `isPrefixOf` x = "\n  "++x
        | otherwise          = x

addCabalProject :: FilePath -> IO ()
addCabalProject dir = do
  writeFile (dir </> "cabal.project") "packages: .\n"

compileModule
    :: FilePath -> NeedsBuildOutput -> ChEntrypoint -> [FilePath] -> [String] -> IO Bool
compileModule pkgdir nb ep srcdirs opts = do
    cwd_before <- getCurrentDirectory
    setCurrentDirectory pkgdir
    flip E.finally (setCurrentDirectory cwd_before) $ do

    putStrLn $ "compiling: " ++ show ep ++ " (" ++ show nb ++ ")"

    E.handle (\(ec :: ExitCode) -> print ec >> return False) $ do

    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do

    runGhc (Just libdir) $ do

    handleSourceError (\e -> GHC.printException e >> return False) $ do

    let target = case nb of
          ProduceBuildOutput -> HscNothing -- AZ: what should this be?
          NoBuildOutput      -> HscInterpreted

    dflags0 <- getSessionDynFlags
    let dflags1 = dflags0 {
        ghcMode   = CompManager
      , ghcLink   = LinkInMemory
      , hscTarget = target
      , optLevel  = 0
      }

    (dflags2, _, _) <- parseDynamicFlags dflags1 (map noLoc opts)
    _ <- setSessionDynFlags dflags2

    ts <- mapM (\t -> guessTarget t Nothing) =<<
         case ep of
           ChLibEntrypoint ms ms' ss -> return $
             map unChModuleName $ ms ++ ms' ++ ss
           ChExeEntrypoint m  ms -> do
             -- TODO: this doesn't take preprocessor outputs in
             -- dist/build/$pkg/$pkg-tmp/ into account.
             m1 <- liftIO $ findFile srcdirs m
             case m1 of
               Just m2 -> return $ [m2] ++ map unChModuleName ms
               Nothing -> error $ printf
                 "Couldn't find source file for Main module (%s), search path:\n\
                 \%s\n" m (show srcdirs)
           ChSetupEntrypoint         -> return $
             -- TODO: this doesn't support Setup.lhs
             ["Setup.hs"]

    let ts' = case nb of
                NoBuildOutput -> map (\t -> t { targetAllowObjCode = False }) ts
                ProduceBuildOutput -> ts

    liftIO $ putStrLn $ "targets: " ++ showPpr dflags2 ts'

    setTargets ts'
    _ <- load LoadAllTargets

    when (nb == NoBuildOutput) $ do
      setContext $ case ep of
        ChLibEntrypoint ms ms' ss ->
            map (IIModule . mkModuleName . unChModuleName) $ ms ++ ms' ++ ss
        ChExeEntrypoint _  ms  ->
            map (IIModule . mkModuleName . unChModuleName) $ ChModuleName "Main" : ms
        ChSetupEntrypoint      ->
            map (IIModule . mkModuleName) ["Main"]

    liftIO $ print ExitSuccess
    return True


data CabalFile = CabalFile FilePath

type ProjSetup0 = ProjSetupDescr (Either SkipReason (Ex ProjSetupImpl))
type ProjSetup1 = ProjSetupDescr (Ex ProjSetupImpl)
type ProjSetup2 pt = ProjSetupDescr (ProjSetupImpl pt)

data ProjSetupDescr a =
  ProjSetupDescr
    { psdHeading :: !String
    , psdImpl    :: !a
    } deriving (Functor)

data ProjSetupImpl pt =
  ProjSetupImpl
    { psiProjType   :: !(SProjType pt)
    , psiDistDir   :: !(FilePath -> DistDir pt)
    , psiProjLoc   :: !(CabalFile -> FilePath -> ProjLoc pt)
    , psiConfigure :: !(FilePath -> IO ())
    , psiBuild     :: !(FilePath -> IO ())
    , psiSdist     :: !(FilePath -> FilePath -> IO ())
    , psiQEmod     :: !(QueryEnv pt -> QueryEnv pt)
    }

oldBuildProjSetup :: ProjSetup0
oldBuildProjSetup = ProjSetupDescr "cabal-v1" $ Right $ Ex $ ProjSetupImpl
    { psiProjType  = SV1
    , psiDistDir   = \dir -> DistDirV1 (dir </> "dist")
    , psiProjLoc   = \(CabalFile cf) _projdir -> ProjLocCabalFile cf
    , psiConfigure = \dir ->
        runWithCwd dir "cabal" [ "configure" ]
    , psiBuild     = \dir ->
        runWithCwd dir "cabal" [ "build" ]
    , psiSdist     = \srcdir destdir ->
        copyMuliPackageProject srcdir destdir (\_ _ -> return ())
    , psiQEmod     = id
    }

newBuildProjSetup :: ProjSetup0
newBuildProjSetup = ProjSetupDescr "cabal-v2" $ Right $ Ex $ ProjSetupImpl
    { psiProjType  = SV2
    , psiDistDir   = \dir  -> DistDirV2 (dir </> "dist-newstyle")
    , psiProjLoc   = \_cabal_file projdir -> ProjLocV2File $ projdir </> "cabal.project"
                     -- TODO: check if cabal.project is there and only use
                     -- V2File then, also remove addCabalProject below so we
                     -- cover both cases.
    , psiConfigure = \dir ->
        runWithCwd dir "cabal" [ "new-configure" ]
    , psiBuild     = \dir ->
        runWithCwd dir "cabal" [ "new-build" ]
    , psiSdist     = \srcdir destdir -> do
        copyMuliPackageProject srcdir destdir $ \pkgsrc pkgdest -> do
          exists <- doesFileExist (pkgsrc </> "cabal.project")
          if exists then
            copyFile (pkgsrc </> "cabal.project") (pkgdest </> "cabal.project")
          else
            addCabalProject pkgdest
    , psiQEmod     = id
    }

stackProjSetup :: Version -> ProjSetup0
stackProjSetup ghcVer =
    ProjSetupDescr "stack" $
    let msg = SkipReason $ "missing stack_resolver_table entry for "++
                           showVersion ghcVer in
    maybe (Left msg) Right $ do
    res <- lookup ghcVer stack_resolver_table
    let argsBefore = [ "--resolver="++res, "--system-ghc" ]
    return $ Ex $ ProjSetupImpl
      { psiProjType  = SStack
      , psiDistDir   = \_dir  -> DistDirStack Nothing
      , psiProjLoc   = \_cabal_file projdir ->
          ProjLocStackYaml $ projdir </> "stack.yaml"
      , psiConfigure = \dir ->
          runWithCwd dir "stack" $ argsBefore ++ [ "build", "--dry-run" ]
      , psiBuild     = \dir ->
          runWithCwd dir "stack" $ argsBefore ++ [ "build" ]
      , psiSdist     = \srcdir destdir -> do
          copyMuliPackageProject srcdir destdir copyStackYamls
      , psiQEmod     = \qe ->
          qe { qePrograms = (qePrograms qe)
               { stackArgsBefore = argsBefore
               }
             }
      }

stack_resolver_table :: [(Version, String)]
stack_resolver_table = map (swap . second parseVer)
  [ ("lts-13.5",  "8.6.3")
  , ("lts-12.26", "8.4.4")
  , ("lts-12.14", "8.4.3")
  , ("lts-11.22", "8.2.2")
  , ("lts-9.21",  "8.0.2")
  , ("lts-7.24",  "8.0.1")
  , ("lts-6.35",  "7.10.3")
  , ("lts-3.22",  "7.10.2")
  ]

copyStackYamls :: FilePath -> FilePath -> IO ()
copyStackYamls srcdir destdir = do
  files <- (\\ [".", ".."]) <$> getDirectoryContents srcdir
  let ymls = filter (".yaml" `isSuffixOf`) $
             filter ("stack-" `isPrefixOf`) $ files
  forM_ ymls $ \filename -> copyFile (srcdir </> filename) (destdir </> filename)

-- | For each Cabal package listed in a @packages.list@ file, copy the package
-- to another directory while only including source files referenced in the
-- cabal file.
copyMuliPackageProject
    :: FilePath -> FilePath -> (FilePath -> FilePath -> IO ()) -> IO ()
copyMuliPackageProject srcdir destdir copyPkgExtra = do
  let packages_file = srcdir </> "packages.list"
  pkgdirs <- lines <$> readFile packages_file
  forM_ pkgdirs $ \pkgdir -> do
    runWithCwd (srcdir </> pkgdir) "cabal"
      [ "act-as-setup", "--", "sdist"
      , "--output-directory="++destdir </> pkgdir ]
    copyPkgExtra (srcdir </> pkgdir) (destdir </> pkgdir)

unChModuleName :: ChModuleName -> String
unChModuleName (ChModuleName  mn) = mn

cabalInstallVersion :: IO Version
cabalInstallVersion =
    parseVer . trim <$> readProcess "cabal" ["--numeric-version"] ""

ghcVersion :: IO Version
ghcVersion =
    parseVer . trim <$> readProcess "ghc" ["--numeric-version"] ""

stackVersion :: IO Version
stackVersion =
    parseVer . trim <$> readProcess "stack" [ "--numeric-version" ] ""

cabalInstallBuiltinCabalVersion :: IO Version
cabalInstallBuiltinCabalVersion =
    parseVer . trim <$> readProcess "cabal"
        ["act-as-setup", "--", "--numeric-version"] ""

-- ---------------------------------------------------------------------
-- | Create and use a temporary directory in the system standard temporary directory.
--
-- Behaves exactly the same as 'withTempDirectory', except that the parent temporary directory
-- will be that returned by 'getCanonicalTemporaryDirectory'.
withSystemTempDirectory' :: String   -- ^ Directory name template
                        -> (FilePath -> IO a) -- ^ Callback that can use the directory
                        -> IO a
withSystemTempDirectory' template action
  = liftIO getCanonicalTemporaryDirectory >>= \tmpDir' -> withTempDirectory' tmpDir' template action

-- | Create and use a temporary directory inside the given directory.
--
-- The directory is deleted after use.
withTempDirectory' :: FilePath -- ^ Parent directory to create the directory in
                  -> String   -- ^ Directory name template
                  -> (FilePath -> IO a) -- ^ Callback that can use the directory
                  -> IO a
withTempDirectory' targetDir template =
  gbracket
    (liftIO (createTempDirectory targetDir template))
    (\x -> return x) -- Leave the dir for inspection later

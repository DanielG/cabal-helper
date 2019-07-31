{-# LANGUAGE TupleSections, ScopedTypeVariables, RecordWildCards, RankNTypes,
  DataKinds, ExistentialQuantification, PolyKinds, ViewPatterns,
  DeriveFunctor, MonoLocalBinds, GADTs, MultiWayIf #-}

{-| This test ensures we can get a GHC API session up and running in a variety of
  project environments.
-}

module Main where

import GHC
import Config
import GHC.Paths (libdir)
import Outputable
import DynFlags

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Data.List
import Data.Maybe
import Data.Version
import Data.Bifunctor
import qualified Data.Map as Map
import System.Console.GetOpt as GetOpt
import System.Environment (getArgs)
import System.Exit
import System.FilePath ((</>), (<.>), makeRelative, takeDirectory)
import System.Directory
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Temp
import Text.Printf (printf)
import Text.Show.Pretty (pPrint)

import Distribution.Helper

import CabalHelper.Shared.Common
import CabalHelper.Compiletime.Types (Env)
import CabalHelper.Compiletime.Process (readProcess, callProcessStderr)
import CabalHelper.Compiletime.Program.GHC
  (GhcVersion(..), ghcVersion)
import CabalHelper.Compiletime.Program.CabalInstall
  (CabalInstallVersion(..), cabalInstallVersion)

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

testConfigToTestSpec :: TestConfig -> ProjType -> String
testConfigToTestSpec (TC loc _ _ _) pt =
  let (topdir, projdir, cabal_file) = testLocPath loc in
  "- " ++ intercalate ":" [topdir, projdir, cabal_file, show pt]

type ModProgs = (Programs -> Programs, CompPrograms -> CompPrograms)

options :: [OptDescr ModProgs]
options =
    [ GetOpt.Option [] ["with-cabal"]
        (ReqArg (\arg -> (\p -> p { cabalProgram = arg }, id)) "PROG")
        "name or path of 'cabal' executable"
    , GetOpt.Option [] ["with-stack"]
        (ReqArg (\arg -> (\p -> p { stackProgram = arg }, id)) "PROG")
        "name or path of 'stack' executable"
    , GetOpt.Option [] ["with-ghc"]
        (ReqArg (\arg -> (id, \cp -> cp { ghcProgram = arg })) "PROG")
        "name or path of 'ghc' executable"
    , GetOpt.Option [] ["with-ghc-pkg"]
        (ReqArg (\arg -> (id, \cp -> cp { ghcPkgProgram = arg })) "PROG")
        "name or path of 'ghc-pkg' executable"
    ]

testOpts :: [String] -> IO (ModProgs, [String])
testOpts args =
   case getOpt Permute options args of
      (o,n,[]  ) -> return (foldl (\(b, d) (a, c) -> (a . b, c . d)) (id, id) o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ghc-session [OPTION..] [TEST_SPEC..]"

main :: IO ()
main = do
  (modProgs, args) <- testOpts =<< getArgs
--  topdir <- getCurrentDirectory

  let withEnv :: (Env => a) -> a
      withEnv action =
        let ?verbose = const False
            ?progs = (fst modProgs) defaultPrograms
            ?cprogs = (snd modProgs) defaultCompPrograms
        in action

  GhcVersion g_ver <- withEnv ghcVersion
  CabalInstallVersion ci_ver <- withEnv cabalInstallVersion
  s_ver <- withEnv stackVersion
    `E.catch` \(_ :: IOError) -> return (makeVersion [0])

  -- Cabal lib version
  f_c_ver :: ProjType -> Either SkipReason Version <- do
    ci_c_ver <- Right <$> withEnv cabalInstallBuiltinCabalVersion
    s_c_ver :: Either SkipReason Version
      <- sequence $ withEnv stackBuiltinCabalVersion s_ver g_ver
    return $ \pt -> case pt of
      V1 -> ci_c_ver
      V2 -> ci_c_ver
      Stack -> s_c_ver

  let showEsrVer = either (\(SkipReason msg) -> "dunno, "++msg) showVersion

  putStrLn "Environment Info"
  putStrLn "================"
  putStrLn ""
  putStrLn $ "cabal-install version: " ++ showVersion ci_ver
  putStrLn $ "cabal-install builtin Cabal version: "
             ++ showEsrVer (f_c_ver V1)
  putStrLn $ "GHC executable version: " ++ showVersion g_ver
  putStrLn $ "GHC library version: " ++ cProjectVersion
  putStrLn $ "Stack version: " ++ showVersion s_ver
  putStrLn $ "Stack Cabal version: " ++ showEsrVer (f_c_ver Stack)
  putStrLn ""

  when (cProjectVersion /= showVersion g_ver) $
    error "GHC version mismatch! See above."

  let proj_impls :: [(ProjType, ProjSetup0)]
      proj_impls =
        -- V2 is sorted before the others here so helper compilation always
        -- uses v2-build caching!
        [ (V2,    newBuildProjSetup)
        , (V1,    oldBuildProjSetup)
        , (Stack, stackProjSetup g_ver)
        ]
      all_proj_types = map fst proj_impls

  tests <- return $ case args of
    xs@(_:_) -> flip map xs $ \loc ->
      let (topdir, ':':x0) = span (/=':') loc
          (projdir0, ':':x1) = span (/=':') x0
          (cabal_file0, ':':pt) = span (/=':') x1
          projdir = makeRelative topdir projdir0
          cabal_file = makeRelative topdir cabal_file0 in
      TC (TF topdir projdir cabal_file) (parseVer "0") (parseVer "0") [read pt]
    [] ->
      -- below V2 is sorted before Stack and V1 since we rely on v2-build's
      -- fucking awesome store cache to keep CI times down.
      --
      -- TODO: Better test coverage for helper compilation with the other two!
      [ TC (TN "exelib")    (parseVer "1.10") (parseVer "0")   []
      , TC (TN "exeintlib") (parseVer "2.0")  (parseVer "0")   []
      , TC (TN "fliblib")   (parseVer "2.0")  (parseVer "0")   []
      , TC (TN "bkpregex")  (parseVer "2.0")  (parseVer "8.1") [V2, V1]
      , let multipkg_loc = TF "tests/multipkg/" "proj/" "proj/proj.cabal" in
        TC  multipkg_loc    (parseVer "1.10") (parseVer "0")   [V2, Stack]
      --            min Cabal lib ver -^    min GHC ver -^
      ]

  putStrLn "Going to Run These Tests"
  putStrLn "========================"
  forM_ tests $ \tc@(TC _ _ _ pts) -> do
    forM_ (if pts == [] then all_proj_types else pts)  $ \pt ->
      putStrLn $ testConfigToTestSpec tc pt

  pPrint tests
  putStrLn ""

  res :: [[TestResult]] <- sequence $ do
    tc@TC {..} <- tests
    (pt, ps0 :: ProjSetup0) <- proj_impls
    guard (null projTypes || pt `elem` projTypes)

    let skip (SkipReason reason) = do
          putStrLn $ intercalate " "
            [ "\n\n\nSkipping test"
            , psdHeading ps0
            , "'" ++ topdir </> projdir_rel ++ "'"
            , "because"
            , reason
            ]
          where
            (topdir, projdir_rel, _) = testLocPath location

    case psdImpl ps0 of
      Left reason -> return $ skip reason >> return []
      Right eximpl -> do
        let ps1 = ps0 { psdImpl = eximpl }
        case checkAndRunTestConfig modProgs VerEnv{..} ps1 tc of
          Left reason -> return $ skip reason >> return []
          Right (Message msg, act) -> return $ putStrLn msg >> act

  putStr "\n\n\n\n"
  putStrLn "Test Results"
  putStrLn "============"
  pPrint res

  if any (==False) $ map trSuccess $ concat res
    then exitFailure
    else exitSuccess

data VerEnv = VerEnv
  { ci_ver :: !Version
  -- ^ cabal-install exe version
  , f_c_ver :: !(ProjType -> Either SkipReason Version)
  -- ^ cabal-install/Stack builtin Cabal library version
  , g_ver  :: !Version
  -- ^ GHC exe version
  , s_ver  :: !Version
  -- ^ Stack exe version
  }

data Message = Message String
data SkipReason = SkipReason String
data TestResult
    = TestResult
      { trSuccess :: Bool
      , trComp    :: ChComponentName
      , trHeading :: String -- ^ project type
      , trDir     :: FilePath
      , trSpec    :: String
      }
 deriving (Show)

testLocPath :: TestLocation -> (FilePath, FilePath, FilePath)
testLocPath (TN test_name) = (projdir, ".", cabal_file)
  where
    projdir :: FilePath
    projdir = "tests" </> test_name
    cabal_file :: FilePath
    cabal_file = test_name <.> "cabal"
testLocPath (TF topdir projdir cabal_file) =
    (topdir, projdir, cabal_file)

-- | Check version bounds of tests against available versions, if successful run
-- the test.
checkAndRunTestConfig
    :: ModProgs
    -> VerEnv
    -> ProjSetup1
    -> TestConfig
    -> Either SkipReason (Message, IO [TestResult])
checkAndRunTestConfig
  modProgs
  VerEnv { ci_ver, f_c_ver, g_ver, s_ver }
  ps1@(psdImpl -> Ex psdImpl2)
  tc@(TC test_loc min_cabal_ver min_ghc_ver _proj_types)
  = let
  pt = demoteSProjType $ psiProjType psdImpl2
  (topdir, projdir_rel, cabal_file) = testLocPath test_loc in do
  c_ver <- f_c_ver pt
  first SkipReason $ do
  if| Stack <- pt, Left (SkipReason msg) <- stackCheckCompat s_ver g_ver ->
      Left $ msg
    | ci_ver < parseVer "1.24" ->
      Left $ "cabal-install-" ++ showVersion ci_ver ++ " is too old"
    | c_ver < min_cabal_ver ->
      Left $ pt_disp pt ++ "'s builtin Cabal version is too old:\n"
             ++ "Cabal-" ++ showVersion c_ver
             ++ " < " ++ showVersion min_cabal_ver
    | g_ver < min_ghc_ver ->
      Left $ "ghc-" ++ showVersion g_ver
             ++ " < " ++ showVersion min_ghc_ver
    | otherwise ->
      Right ()
  return $ (,)
    (Message $ intercalate " "
      [ "\n\n\nRunning test"
      , psdHeading ps1
      , "'" ++ topdir ++ "'"
      ])
    $ do
      trs <- runTest
        modProgs ps1{ psdImpl = psdImpl2 } topdir projdir_rel cabal_file
      return $ map ($ testConfigToTestSpec tc pt) trs

  where
    pt_disp V1 = "cabal-install"
    pt_disp V2 = "cabal-install"
    pt_disp Stack = "Stack"


runTest
    :: ModProgs -> ProjSetup2 pt
    -> FilePath -> FilePath -> FilePath
    -> IO [String -> TestResult]
runTest modProgs ps2@(psdImpl -> ProjSetupImpl{..}) topdir projdir cabal_file
  = do
  withSystemTempDirectory' "cabal-helper.ghc-session.test" $ \tmpdir -> do
    trs <- test modProgs ps2 topdir tmpdir (tmpdir </> projdir) (tmpdir </> cabal_file)
    return $
      map ($ (topdir </> projdir)) $
      map ($ (psdHeading ps2)) $
      trs

runWithCwd :: FilePath -> String -> [String] -> IO ()
runWithCwd cwd x xs = do
  let ?verbose = (==1)
  callProcessStderr (Just cwd) x xs

run :: String -> [String] -> IO ()
run x xs = do
  let ?verbose = (==1)
  callProcessStderr Nothing x xs

test
    :: ModProgs -> ProjSetup2 pt
    -> FilePath -> FilePath -> FilePath -> FilePath
    -> IO [(String -> String -> FilePath -> TestResult)]
test modProgs (psdImpl -> ProjSetupImpl{..}) topdir tmpdir projdir cabal_file
  = do
    qe' <- psiQEmod <$> mkQueryEnv
            (psiProjLoc (CabalFile cabal_file) projdir)
            (psiDistDir projdir)

    let qe = qe' { qePrograms = (fst modProgs) (qePrograms qe')
                 , qeCompPrograms = (snd modProgs) (qeCompPrograms qe')
                 }
        progs = qePrograms qe

    psiSdist progs topdir tmpdir
    psiConfigure progs projdir

    cs <- concat <$> runQuery (allUnits (Map.elems . uiComponents)) qe

    when (any ((==ProduceBuildOutput) . ciNeedsBuildOutput) cs) $
      psiBuild progs projdir

    let pkgdir = takeDirectory cabal_file
    homedir <- getHomeDirectory
    let var_table =
          [ (pkgdir,  "${pkgdir}")
          , (homedir, "${HOME}")
          ]

    forM cs $ \ChComponentInfo{..} -> do
        let opts' = "-Werror" : ciGhcOptions
        let sopts = intercalate " " $ map formatArg $ "ghc" : map (normalizeOutputWithVars var_table) opts'

        putStrLn $ "\n" ++ show ciComponentName ++ ":\n"
        hPutStrLn stderr $ "cd " ++ pkgdir -- messes up normalized output
        putStrLn sopts

        hFlush stdout
        tr <- compileModule pkgdir ciNeedsBuildOutput ciEntrypoints ciSourceDirs opts'
        return $ tr ciComponentName
  where
    formatArg x
        | "-" `isPrefixOf` x = "\\\n  "++x
        | otherwise          = x

addCabalProject :: FilePath -> IO ()
addCabalProject dir = do
  writeFile (dir </> "cabal.project") "packages: .\n"

compileModule
    :: FilePath -> NeedsBuildOutput -> ChEntrypoint -> [FilePath] -> [String]
    -> IO (ChComponentName -> FilePath -> String -> String -> TestResult)
compileModule pkgdir nb ep srcdirs opts = do
    cwd_before <- getCurrentDirectory
    setCurrentDirectory pkgdir
    flip E.finally (setCurrentDirectory cwd_before) $ do

    putStrLn $ "compiling: " ++ show ep ++ " (" ++ show nb ++ ")"

    E.handle (\(ec :: ExitCode) -> print ec >> return (TestResult False)) $ do

    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
    let printGhcEx e = GHC.printException e >> return (TestResult False)
    handleSourceError printGhcEx $ do

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

    return $ TestResult True


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
    { psiProjType  :: !(SProjType pt)
    , psiDistDir   :: !(FilePath -> DistDir pt)
    , psiProjLoc   :: !(CabalFile -> FilePath -> ProjLoc pt)
    , psiConfigure :: !(Programs -> FilePath -> IO ())
    , psiBuild     :: !(Programs -> FilePath -> IO ())
    , psiSdist     :: !(Programs -> FilePath -> FilePath -> IO ())
    , psiQEmod     :: !(QueryEnv pt -> QueryEnv pt)
    }

oldBuildProjSetup :: ProjSetup0
oldBuildProjSetup = ProjSetupDescr "cabal-v1" $ Right $ Ex $ ProjSetupImpl
    { psiProjType  = SCabal SCV1
    , psiDistDir   = \dir -> DistDirCabal SCV1 (dir </> "dist")
    , psiProjLoc   = \(CabalFile cf) projdir -> ProjLocV1CabalFile cf projdir
    , psiConfigure = \progs dir ->
        runWithCwd dir (cabalProgram progs) [ "configure" ]
    , psiBuild     = \progs dir ->
        runWithCwd dir (cabalProgram progs) [ "build" ]
    , psiSdist     = \progs srcdir destdir ->
        copyMuliPackageProject progs srcdir destdir (\_ _ -> return ())
    , psiQEmod     = id
    }

newBuildProjSetup :: ProjSetup0
newBuildProjSetup = ProjSetupDescr "cabal-v2" $ Right $ Ex $ ProjSetupImpl
    { psiProjType  = SCabal SCV2
    , psiDistDir   = \dir  -> DistDirCabal SCV2 (dir </> "dist-newstyle")
    , psiProjLoc   = \_cabal_file projdir -> ProjLocV2File $ projdir </> "cabal.project"
                     -- TODO: check if cabal.project is there and only use
                     -- V2File then, also remove addCabalProject below so we
                     -- cover both cases.
    , psiConfigure = \progs dir ->
        runWithCwd dir (cabalProgram progs) [ "new-configure" ]
    , psiBuild     = \progs dir ->
        runWithCwd dir (cabalProgram progs) [ "new-build" ]
    , psiSdist     = \progs srcdir destdir -> do
        copyMuliPackageProject progs srcdir destdir $ \pkgsrc pkgdest -> do
          exists <- doesFileExist (pkgsrc </> "cabal.project")
          if exists then
            copyFile (pkgsrc </> "cabal.project") (pkgdest </> "cabal.project")
          else
            addCabalProject pkgdest
    , psiQEmod     = id
    }

stackProjSetup :: Version -> ProjSetup0
stackProjSetup ghcVer =
    ProjSetupDescr "stack" $ do
    res <- lookupStackResolver ghcVer
    let argsBefore = [ "--resolver="++res, "--system-ghc" ]
    return $ Ex $ ProjSetupImpl
      { psiProjType  = SStack
      , psiDistDir   = \_dir  -> DistDirStack Nothing
      , psiProjLoc   = \_cabal_file projdir ->
          ProjLocStackYaml $ projdir </> "stack.yaml"
      , psiConfigure = \progs dir ->
          runWithCwd dir (stackProgram progs) $ argsBefore ++ [ "build", "--dry-run" ]
      , psiBuild     = \progs dir ->
          runWithCwd dir (stackProgram progs) $ argsBefore ++ [ "build" ]
      , psiSdist     = \progs srcdir destdir -> do
          copyMuliPackageProject progs srcdir destdir copyStackYamls
      , psiQEmod     = \qe ->
          qe { qePrograms = (qePrograms qe)
               { stackArgsBefore = argsBefore
               }
             }
      }

lookupStackResolver :: Version -> Either SkipReason String
lookupStackResolver ghcVer = maybe (Left msg) Right $
    lookup ghcVer stack_resolver_table
  where
    msg = SkipReason $ "missing stack_resolver_table entry for "++
                       showVersion ghcVer

stack_resolver_table :: [(Version, String)] -- ^ (ghc version, stack resolver)
stack_resolver_table = unsafePerformIO $
  map (\(words -> [g, l]) -> (parseVer g, l)) . lines
  <$> readFile "tests/stack-resolvers"

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
    :: Programs -> FilePath -> FilePath -> (FilePath -> FilePath -> IO ()) -> IO ()
copyMuliPackageProject progs srcdir destdir copyPkgExtra = do
  let packages_file = srcdir </> "packages.list"
  pkgdirs <- lines <$> readFile packages_file
  forM_ pkgdirs $ \pkgdir -> do
    runWithCwd (srcdir </> pkgdir) (cabalProgram progs)
      [ "act-as-setup", "--", "sdist"
      , "--output-directory="++destdir </> pkgdir ]
    copyPkgExtra (srcdir </> pkgdir) (destdir </> pkgdir)

stackVersion :: (?progs :: Programs) => IO Version
stackVersion =
  parseVer . trim <$> readProcess (stackProgram ?progs) [ "--numeric-version" ] ""

stackBuiltinCabalVersion
    :: (?progs :: Programs)
    => Version -> Version -> Either SkipReason (IO Version)
stackBuiltinCabalVersion s_ver g_ver = do
    _ <- stackCheckCompat s_ver g_ver
    res <- lookupStackResolver g_ver
    return $ parseVer . trim <$> readProcess (stackProgram ?progs)
        [ "--resolver="++res, "--system-ghc", "exec", "--"
        , "ghc-pkg", "--simple-output", "field", "Cabal", "version"
        ] ""

stackCheckCompat :: Version -> Version -> Either SkipReason ()
stackCheckCompat s_ver g_ver =
  if| s_ver < parseVer "1.9.4" ->
      if| g_ver >= parseVer "8.2.2" ->
          error $ printf
            "stack-%s is too old, but GHC %s is recent enough to build it.\n\
            \The CI scripts should have installed it! See 25-deps.sh\n"
            (showVersion s_ver) (showVersion g_ver)
        | otherwise ->
          Left $ SkipReason $ "stack-" ++ showVersion s_ver ++ " is too old"
    | otherwise ->
        Right ()

cabalInstallBuiltinCabalVersion :: (?progs :: Programs) => IO Version
cabalInstallBuiltinCabalVersion =
    parseVer . trim <$> readProcess (cabalProgram ?progs)
        ["act-as-setup", "--", "--numeric-version"] ""

normalizeOutputWithVars :: [(String, String)] -> String -> String
normalizeOutputWithVars ts str =
  case filter (isJust . fst) $ map (first (flip stripPrefix str)) ts of
    (Just rest, replacemnet) : _ ->
        replacemnet ++ normalizeOutputWithVars ts rest
    _ -> cont
  where
    cont =
      case str of
        s:ss -> s : normalizeOutputWithVars ts ss
        [] -> []
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

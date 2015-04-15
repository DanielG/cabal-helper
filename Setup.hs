#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Install
import Distribution.Simple.Register
import Distribution.Simple.InstallDirs as ID
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import System.FilePath

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
         instHook = inst,
         copyHook = copy
       }

-- mostly copypasta from 'defaultInstallHook'
inst ::
    PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
inst pd lbi _uf ifl = do
  let copyFlags = defaultCopyFlags {
                      copyDistPref   = installDistPref ifl,
                      copyDest       = toFlag NoCopyDest,
                      copyVerbosity  = installVerbosity ifl
                  }
  xInstallTarget pd lbi (\pd' lbi' -> install pd' lbi' copyFlags)
  let registerFlags = defaultRegisterFlags {
                          regDistPref  = installDistPref ifl,
                          regInPlace   = installInPlace ifl,
                          regPackageDB = installPackageDB ifl,
                          regVerbosity = installVerbosity ifl
                      }
  when (hasLibs pd) $ register pd lbi registerFlags

copy :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copy pd lbi _uh cf =
    xInstallTarget pd lbi (\pd' lbi' -> install pd' lbi' cf)



xInstallTarget :: PackageDescription
               -> LocalBuildInfo
               -> (PackageDescription -> LocalBuildInfo -> IO ())
               -> IO ()
xInstallTarget pd lbi fn = do
  let (extended, regular) = partition (isJust . installTarget) (executables pd)

  let pd_regular = pd { executables = regular }

  _ <- flip mapM extended $ \exe -> do
    putStrLn $ "extended "  ++ show (exeName exe)

    let
        idirtpl          = installDirTemplates lbi
        env              = installDirsTemplateEnv idirtpl
        libexecdir'      = fromPathTemplate (libexecdir idirtpl)

        pd_extended      = onlyExePackageDesc [exe] pd
        install_target   = fromJust $ installTarget exe
        install_target'  = ID.substPathTemplate env install_target
        -- $libexec isn't a real thing :/ so we have to simulate it
        install_target'' = substLibExec' libexecdir' install_target'

    let lbi' = lbi {
                 installDirTemplates =
                     (installDirTemplates lbi) {
                   bindir = install_target''
                 }
               }
    fn pd_extended lbi'

  fn pd_regular lbi

 where
   installTarget :: Executable -> Maybe PathTemplate
   installTarget exe =
    toPathTemplate <$> lookup "x-install-target" (customFieldsBI $ buildInfo exe)

   substLibExec libexecdir "$libexecdir" = libexecdir
   substLibExec _ comp = comp

   substLibExec' dir =
       withPT $
           withSP $ map (substLibExec dir . dropTrailingPathSeparator)


   withPT f pt = toPathTemplate $ f (fromPathTemplate pt)
   withSP f p  = joinPath $ f (splitPath p)

onlyExePackageDesc :: [Executable] -> PackageDescription -> PackageDescription
onlyExePackageDesc exes pd = emptyPackageDescription {
                     package = package pd
                   , executables = exes
                   }

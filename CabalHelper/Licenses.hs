{-# LANGUAGE CPP #-}
module CabalHelper.Licenses where

-- Copyright (c) 2014, Jasper Van der Jeugt <m@jaspervdj.be>

--------------------------------------------------------------------------------
import           Control.Arrow                      ((***), (&&&))
import           Control.Monad                      (forM_, unless)
import           Data.List                          (foldl', sort)
import           Data.Maybe                         (catMaybes)
import           Data.Version                       (Version)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Distribution.InstalledPackageInfo  (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo  as InstalledPackageInfo
import qualified Distribution.License               as Cabal
import qualified Distribution.Package               as Cabal
import qualified Distribution.Simple.Configure      as Cabal
import qualified Distribution.Simple.LocalBuildInfo as Cabal
import qualified Distribution.Simple.PackageIndex   as Cabal
import qualified Distribution.Text                  as Cabal
import           System.Directory                   (getDirectoryContents)
import           System.Exit                        (exitFailure)
import           System.FilePath                    (takeExtension)
import           System.IO                          (hPutStrLn, stderr)

--------------------------------------------------------------------------------

#if CABAL_MAJOR == 1 && CABAL_MINOR >= 22
type PackageIndex a = Cabal.PackageIndex (InstalledPackageInfo.InstalledPackageInfo_ a)
#else
type PackageIndex a = Cabal.PackageIndex
#endif

findTransitiveDependencies
    :: PackageIndex a
    -> Set Cabal.InstalledPackageId
    -> Set Cabal.InstalledPackageId
findTransitiveDependencies pkgIdx set0 = go Set.empty (Set.toList set0)
  where
    go set []  = set
    go set (q : queue)
        | q `Set.member` set = go set queue
        | otherwise          =
            case Cabal.lookupInstalledPackageId pkgIdx q of
                Nothing  ->
                    -- Not found can mean that the package still needs to be
                    -- installed (e.g. a component of the target cabal package).
                    -- We can ignore those.
                    go set queue
                Just ipi ->
                    go (Set.insert q set)
                        (InstalledPackageInfo.depends ipi ++ queue)


--------------------------------------------------------------------------------
getDependencyInstalledPackageIds
    :: Cabal.LocalBuildInfo -> Set Cabal.InstalledPackageId
getDependencyInstalledPackageIds lbi =
    findTransitiveDependencies (Cabal.installedPkgs lbi) $
      Set.fromList $ map fst $ Cabal.externalPackageDeps lbi

--------------------------------------------------------------------------------
getDependencyInstalledPackageInfos
    :: Cabal.LocalBuildInfo -> [InstalledPackageInfo]
getDependencyInstalledPackageInfos lbi = catMaybes $
    map (Cabal.lookupInstalledPackageId pkgIdx) $
    Set.toList (getDependencyInstalledPackageIds lbi)
  where
    pkgIdx = Cabal.installedPkgs lbi


--------------------------------------------------------------------------------
groupByLicense
    :: [InstalledPackageInfo]
    -> [(Cabal.License, [InstalledPackageInfo])]
groupByLicense = foldl'
    (\assoc ipi -> insert (InstalledPackageInfo.license ipi) ipi assoc) []
  where
    -- 'Cabal.License' doesn't have an 'Ord' instance so we need to use an
    -- association list instead of 'Map'. The number of licenses probably won't
    -- exceed 100 so I think we're alright.
    insert :: Eq k => k -> v -> [(k, [v])] -> [(k, [v])]
    insert k v []   = [(k, [v])]
    insert k v ((k', vs) : kvs)
        | k == k'   = (k, v : vs) : kvs
        | otherwise = (k', vs) : insert k v kvs


--------------------------------------------------------------------------------
displayDependencyLicenseList
    :: [(Cabal.License, [InstalledPackageInfo])]
    -> [(String, [(String, Version)])]
displayDependencyLicenseList =
    map (Cabal.display *** map (getName &&& getVersion))
  where
    getName =
        Cabal.display . Cabal.pkgName . InstalledPackageInfo.sourcePackageId
    getVersion =
        Cabal.pkgVersion . InstalledPackageInfo.sourcePackageId

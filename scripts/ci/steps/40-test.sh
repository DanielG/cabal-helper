
cabal --builddir="$build_dir" new-run ghc-session

cabal --builddir="$build_dir" new-run compile-test

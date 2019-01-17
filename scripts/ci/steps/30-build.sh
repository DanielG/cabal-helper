# -fdev enables building the helper "main" exe directly and enables more warnings
cabal new-configure --builddir="$build_dir" -fdev --enable-tests
cabal new-build     --builddir="$build_dir"
cabal new-haddock   --builddir="$build_dir"

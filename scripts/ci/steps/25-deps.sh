
verlte() {
    [  "$1" = "$(printf '%s\n%s\n' "$1" "$2" | sort -V | head -n1)" ]
}

mkdir -p "$build_dir/bin"


if command -v stack; then
	stack_ver=$(stack --numeric-version)
else
	stack_ver=0
fi

ghc_ver=$(ghc --numeric-version)

# we need at least 1.9.4 for `stack ide packages --cabal-files --stdout`
if verlte "$stack_ver" 1.9.3 &&
   verlte 8.2.2 "$ghc_ver"
then
	PATH="$build_dir/bin:$PATH"
	export PATH

	cabal v2-install \
	      --symlink-bindir="$build_dir/bin" \
	      --constraint "network < 3" \
	      --package-env=/dev/null \
	      hpack || exit 1

	stack_dir="$(mktemp --tmpdir -d "cabal-helper.stacksrcXXXXXXXXX")"

	git clone \
	    --depth=1 \
	    --branch=stable \
	    https://github.com/commercialhaskell/stack "$stack_dir"  || exit 1

	(
		cd "$stack_dir"
		hpack
		cabal v2-install \
		      --symlink-bindir="$build_dir/bin" \
		      --constraint "Cabal == 2.4.0.1" \
		      --constraint "network < 3" . \
		      --package-env=/dev/null || exit 1
        ) || exit 1
fi


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
        "$CI_SCRIPTS_DIR"/install-stack.sh "$build_dir/bin"
	PATH="$build_dir/bin:$PATH"
	export PATH
fi

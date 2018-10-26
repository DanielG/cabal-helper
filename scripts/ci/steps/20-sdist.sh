mkdir -p "$source_dir"
mkdir -p "$build_dir"

cabal act-as-setup -- sdist --output-directory="$source_dir"

if [ -d cabal-plan/ ]; then
        (
            cd cabal-plan/
            cabal act-as-setup -- sdist --output-directory="$source_dir/cabal-plan"
        )
fi

cd "$source_dir"

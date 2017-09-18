source_dir="$(mktemp --tmpdir -d "cabal-helper.sdistXXXXXXXXX")"
mkdir -p "$source_dir"

cabal sdist --output-directory="$source_dir"

if [ -e cabal.sandbox.config ]; then
    cp cabal.sandbox.config "$source_dir"
fi

cd "$source_dir"

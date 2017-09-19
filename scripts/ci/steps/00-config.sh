if [ -w . ]; then
        sandbox=./.cabal-sandbox
        sandbox_config=./cabal.sandbox.config
else
        sandbox=$HOME/cabal-sandbox
        sandbox_config=$HOME/cabal.sandbox.config
fi

source_dir="$(mktemp --tmpdir -d "cabal-helper.sdistXXXXXXXXX")"
build_dir="$(mktemp --tmpdir -d "cabal-helper.distXXXXXXXXX")"

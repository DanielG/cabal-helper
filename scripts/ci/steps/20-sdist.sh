mkdir -p "$source_dir"
mkdir -p "$build_dir"

cabal act-as-setup -- sdist --output-directory="$source_dir"
cp cabal.project "$source_dir"/cabal.project

if [ -d cabal-plan/ ]; then
        (
            cd cabal-plan/
            cabal act-as-setup -- sdist --output-directory="$source_dir/cabal-plan"
        )
fi

cd "$source_dir"

git init
git config --local user.email "$USER@$(hostname)"
git config --local user.name "cabal-helper CI"
git add --all
git commit -m .

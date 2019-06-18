#!/bin/sh
# Example:
#     $ scripts/ci/update-cabal-versions.sh | tee tests/cabal-versions

wget -q -O- https://hackage.haskell.org/package/Cabal/preferred.json \
        | jq -r '."normal-version"[]' | sort -V

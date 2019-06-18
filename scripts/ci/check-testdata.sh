#!/bin/bash

set -e -o pipefail

echo "=== Stack resolvers"

$CI_PROJECT_DIR/scripts/ci/update-stack-resolvers.sh \
        | tee tests/stack-resolvers.new

git diff --exit-code -- tests/stack-resolvers tests/stack-resolvers.new \
        && echo OK

echo; echo; echo "=== Cabal versions"
$CI_PROJECT_DIR/scripts/ci/update-cabal-versions.sh \
        | tee tests/cabal-versions.new

git diff --exit-code -- tests/cabal-versions tests/cabal-versions.new \
        && echo OK

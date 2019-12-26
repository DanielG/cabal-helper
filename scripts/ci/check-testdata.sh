#!/bin/bash

CI_PROJECT_DIR=${CI_PROJECT_DIR:-.}
rv=0

set -e -o pipefail

echo "=== Stack resolvers"

$CI_PROJECT_DIR/scripts/ci/update-stack-resolvers.sh \
        | tee tests/stack-resolvers.new

if diff --from-file=tests/stack-resolvers tests/stack-resolvers.new; then
        echo OK
else
        echo FAIL stack
        rv=1
fi

echo; echo; echo "=== Cabal versions"
$CI_PROJECT_DIR/scripts/ci/update-cabal-versions.sh \
        | tee tests/cabal-versions.new

if diff --from-file=tests/cabal-versions tests/cabal-versions.new; then
        echo OK
else
        echo FAIL cabal
        rv=1
fi

exit $rv

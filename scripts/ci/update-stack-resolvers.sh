#!/bin/sh
# Example:
#     $ scripts/ci/update-stack-resolvers.sh | tee tests/stack-resolvers

mkdir -p /tmp/stack-resolvers/

wget -q https://s3.amazonaws.com/haddock.stackage.org/snapshots.json \
     -O /tmp/stack-resolvers/snapshots.json

resolvers=$(cat /tmp/stack-resolvers/snapshots.json \
                    | jq -r '.[]' | grep ^lts- | sort -V -r | uniq)

for res in $resolvers; do
        wget -q --continue \
             -O /tmp/stack-resolvers/$res.yaml \
             https://raw.githubusercontent.com/fpco/lts-haskell/master/$res.yaml

        ghc=$(cat /tmp/stack-resolvers/$res.yaml \
                      | grep ghc-version | awk '{ print $2 }' | tr -cd '0-9.')

        printf '%-10s %s\n' "$ghc" "$res"
done

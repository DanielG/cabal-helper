#!/bin/sh
# Example:
#     $ scripts/ci/update-stack-resolvers.sh | tee tests/stack-resolvers

STACKAGE_SNAPSHOTS_REPO=https://github.com/commercialhaskell/stackage-snapshots

tmp=/tmp/stack-resolvers
mkdir -p "$tmp"

wget -q -O- "$STACKAGE_SNAPSHOTS_REPO"/archive/master.tar.gz \
        | tar -xvz --strip-components=1 --show-transformed-names \
        | tee "$tmp"/all-resolvers.list

< "$tmp"/all-resolvers.list \
  sed -rn 's_((lts)/([0-9]+)/([0-9]+)\.yaml)_\1 \2-\3.\4 \2-\3_p' \
        | sort -V -r | uniq -f 2 > "$tmp"/resolvers.list

while read -r path lts _; do
        ghc=$(cat $path | sed -rn 's/^ *compiler: +ghc-([0-9.]+)/\1/p')
        printf '%-10s %s\n' "$ghc" "$lts"
done < "$tmp"/resolvers.list

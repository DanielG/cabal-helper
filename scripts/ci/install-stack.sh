#!/bin/sh

BINDIR="$1"; shift

cabal v2-install \
      --symlink-bindir="$BINDIR" \
      --constraint "network < 3" \
      --package-env=/dev/null \
      hpack || exit 1

stack_dir="$(mktemp --tmpdir -d "install-stack.XXXXXXXXX")"
trap 'rm -rf '"$stack_dir" 0 2 15

git clone \
    --depth=1 \
    --branch=stable \
    https://github.com/commercialhaskell/stack "$stack_dir"  || exit 1

(
	cd "$stack_dir"
	"$BINDIR/hpack"
	cabal v2-install \
	      --symlink-bindir="$BINDIR" \
	      --constraint "Cabal == 2.4.0.1" \
	      --constraint "network < 3" . \
	      --package-env=/dev/null || exit 1
) || exit 1

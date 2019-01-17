
source_dir="$(mktemp --tmpdir -d "cabal-helper.sdistXXXXXXXXX")"
build_dir="$(mktemp --tmpdir -d "cabal-helper.distXXXXXXXXX")"

NPROC=${NPROC:-1}

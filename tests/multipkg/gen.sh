#!/bin/sh

printf '' > package-paths.list

while read -r path name deps; do
    mkdir -p "$path"
    printf '%s\n' "$path" >> package-paths.list
    cat > "$path/$name.cabal" <<EOF
name:                ${name}
version:             0
build-type:          Simple
cabal-version:       >=1.10

library
  exposed-modules:   Lib
  build-depends:     base, filepath, directory ${deps}
  default-language:  Haskell2010

executable ${name}-exe
  main-is:           Exe.hs
  build-depends:     base, ${name} ${deps}
  default-language:  Haskell2010

test-suite ${name}-test
  type:              exitcode-stdio-1.0
  main-is:           Exe.hs
  build-depends:     base, ${name} ${deps}

benchmark ${name}-bench
  type:              exitcode-stdio-1.0
  main-is:           Exe.hs
  build-depends:     base, ${name} ${deps}
EOF
done <<EOF
proj/       proj     ,pkg-a,pkg-b,pkg-oot
proj/pkg-a  pkg-a
proj/pkg-b  pkg-b
pkg-oot/    pkg-oot
EOF

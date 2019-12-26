# The `cabal-helper` library
[![build status](https://gitlab.com/dxld/cabal-helper/badges/master/pipeline.svg)](https://gitlab.com/dxld/cabal-helper/commits/master)

The purpose of the `cabal-helper` library is to give Haskell development
tools access to the same environment which build tools such as
[cabal](https://www.haskell.org/cabal) and
[stack](https://www.haskellstack.org) normally provide to the compiler.

## Introduction

In the Haskell ecosystem the most widely used [build system](#build-system) is
the [`Cabal` library](https://hackage.haskell.org/package/Cabal), not to be
confused with the `cabal` *build tool* which is usually refered to by it's
package-name: `cabal-install` to disambiguate.

All contemporary meta *build tools* such as `cabal` and `stack` as well as some
custom Haskell [build systems](#build-system) use the `Cabal` library as their
foundation. For example the Glasgow Haskell Compiler's bespoke GNU Make based
build system also utilises the `Cabal` library at its core.

We capitalize on this fact by using build information `Cabal` writes to disk as
the common denominator between all Haskell *build tools*. This allows us to
easily support a variety of *build tools* without incuring significant
additional complexity.

## Technical Background

### Haskell Packages and `Setup.hs`

Essentially all Haskell packages implement
["The Haskell Cabal" (pdf)](https://www.haskell.org/cabal/proposal/pkg-spec.pdf)
packaging specification. The `Cabal` library and `cabal-install` *build tool*
are named after this specification. Yes we really love confusing naming in
Haskell land.

The specification revolves around this `Setup.hs` script file you might
have seen before. Basically the idea is a Haskell source package consists
of, at the very least, a `Setup.hs` file, which is a Haskell program that
provides a well defined command-line interface for configuring, building
and installing it. Haskell developers can use *build tools*, such as
`cabal`, which interface with `Setup.hs` and provide functionality on top
it.

Note that even though originally packages were meant to be free to
implement the `Setup.hs` interface however they like, this hasn't been
supported by build tools for a long time. In practice use of the `Cabal`
library in `Setup.hs` is mandatory.

Now, the first step of building a package in "The Haskell Cabal" is to call
`Setup.hs`'s `configure` command:

    $ runhaskell Setup.hs configure

When invoking `Setup.hs` the default behaviour of the `Cabal` library is to
first read the `<pkg-name>.cabal` configuration file to determine how the
project is structured and what dependencies it has. In the case of
`configure` it will then probe the system it's running on about:

 - the list of available Haskell packages,
 - system package dependencies (using pkg-config) and
 - Haskell compiler type, version and supported language extensions (among
   other things).

Finally Haskell package dependency resolution is also run.
 
`Cabal` then writes all the gathered information on the concrete
configuration of the package into a file called `setup-config`. Subsequent
steps, such as`./Setup.hs build`, will then read this state file instead of
`<package-name>.cabal` to avoid having to probing the system or run
dependency resolution again.

It is this file that `cabal-helper` is primarily concerned with reading and
presenting in a usable manner. Reading this file essentially means that all
the complicated work has already been done for us and we can get straight
to running the compiler.

### Multi-package projects

So far so good. That's pretty much the end of the story for the traditional
`cabal build` commands but what about `cabal new-build` and Stack I hear
you asking?

Well, essentially both new-build and Stack simply build on top of the
traditional `Setup.hs` interface. So the `setup-config` file is still there
in all it's glory, we just have to deal with more than one of it since both
build-tools support multiple packages and `Setup.hs` only knows how to deal
with a single package at a time.

To support this cabal-helper has grown a representation of what a project
is in it's API starting with the 1.0 series. We currently support both
new-build and Stack. The API is designed to allow extending support to
custom build systems such as GHC's but we have not done this yet.

### The "Helper" in cabal-helper

In the API docs you will find frequent mentions of "the helper executable"
so I'll explain what that is here because it is quite fundamental to how
things work in the codebase.

The fundamental problem cabal-helper solves is the fact that in order to
access the data Cabal stores in the `setup-config` file we have to link
against `lib:Cabal`. However the binary format of this file is unstable and
there is no backwards compatibility mechanism in the library. So to read a
`setup-config` file produced by a certain version of Cabal we have to link
against exactly that version.

Not only that but usally the `cabal` commandline tool controls which Cabal
library version is used, so we really just have to deal with whatever we
get.

To solve this problem the cabal-helper library builds a small executable at
runtime who's only purpose is to link against `lib:Cabal`, read the
contents of `setup-config` and present the data there in a Cabal version
independent format for consumption by the cabal-helper library.

Recently some work was merged into cabal to have `Setup.hs` to do this
natively (https://github.com/haskell/cabal/pull/5954), we're planning to
use this eventually to replace our "helper".

## IRC

If you have any problems, suggestions, comments swing by
[\#ghc-mod (web client)](https://kiwiirc.com/client/irc.freenode.org/ghc-mod) on
Freenode. If you're reporting a bug please also create an issue
[here](https://github.com/DanielG/cabal-helper/issues) so we have a way to
contact you if you don't have time to stay.

Do hang around for a while if no one answers and repeat your question if you
still haven't gotten any answer after a day or so. You're most likely to get an
answer during the day in GMT+1.

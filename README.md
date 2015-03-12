# cabal-helper

`cabal-helper` provides a library which wraps the internal use of executables
to lift the restrictions imposed by linking against versions of GHC before
`7.10`.

`cabal-helper` uses a wrapper executable to compile the actual cabal-helper
executable at runtime while linking against an arbitrary version of
Cabal. This runtime-compiled helper executable is then used to extract
various bits and peices from Cabal on disk state (dist/setup-config) written
by it's configure command.

In addition to this the wrapper executable also supports installing any
version of Cabal from hackage in case it cannot be found in any available
package database. The wrapper installs these instances of the Cabal library
into a private package database so as to not interfere with the user's
packages.

Furthermore the wrapper supports one special case namely reading a state
file for Cabal itself. This is needed as Cabal compiles it's Setup.hs using
itself and not using any version of Cabal installed in any package database.

Currently `cabal-helper` supports @Cabal >= 1.16@.

## IRC

If you have any problems, suggestions, comments swing by
[\#ghc-mod (web client)](https://kiwiirc.com/client/irc.freenode.org/ghc-mod) on
Freenode. If you're reporting a bug please also create an issue
[here](https://github.com/DanielG/cabal-helper/issues) so we have a way to
contact you if you don't have time to stay.

Do hang around for a while if no one answers and repeat your question if you
still haven't gotten any answer after a day or so. You're most likely to get an
answer during the day in GMT+1.

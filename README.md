Theseus
=======

A labyrinth game inspired by the Monad.Reader special issue on Poetry and fiction.

Source for the game can be found at [github](https://github.com/benjaminfjones/theseus).

Build Instructions
------------------

The game can be built using [cabal](http://www.haskell.org/cabal) with the following
command:

    $ cabal install --prefix=$HOME --user

This will install the executable ``theseus`` in ``$HOME/bin``.

Documentation
-------------

Documentation for the game can be built using the following command:

    $ cabal --executables haddock

The resulting HTML docs can be viewed at ``dist/doc/html/theseus/theseus/index.html``.

Running Builtin Tests
---------------------

Built in tests (using the QuickCheck module) can be run with the following command:

    $ runhaskell Tests.hs


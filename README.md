# hsfmt
A Haskell code formatter using prettyprinter and the GHC API

This project is not complete. Don't tell me you don't like how it looks, because neither do I. We'll get there. However, if you want to send a pull request that changes formatting, by all means go ahead! Just make sure the tests all pass :)


## Usage

Right now, `hs-fmt` just formats a single file, and prints out the result to `STDOUT`.


    $ stack build
    $ stack exec -- hs-fmt FILENAME

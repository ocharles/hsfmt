module Main where

import System.Environment (getArgs)
import System.Exit (die)

import HSFmt (prettyPrintFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> die "Please specify a filename."
    (f:_) -> prettyPrintFile f >>= putStrLn

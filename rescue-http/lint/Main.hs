module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit

arguments :: [String]
arguments =
    [ "benchmark"
    , "library"
    , "test"
    , "doctest"
    ]

main :: IO ()
main = do
  hints <- hlint arguments
  if null hints
    then exitSuccess
    else exitFailure

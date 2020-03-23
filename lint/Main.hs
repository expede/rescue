module Main (main) where

import Language.Haskell.HLint (hlint)

arguments :: [String]
arguments =
    [ "benchmark"
    , "app"
    , "library"
    , "test"
    ]

main :: IO ()
main = do
  hints <- hlint arguments
  if null hints
    then exitSuccess
    else exitFailure

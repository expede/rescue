module Main where

import Criterion
import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "Textable.encode" encode
  ]

encode :: [Benchmark]
encode = []

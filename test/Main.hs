module Main (main) where

import Test.Tasty

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = pure []

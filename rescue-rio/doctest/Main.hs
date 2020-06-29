module Main (main) where

import           System.FilePath.Glob (glob)

import           Test.DocTest (doctest)
import           Test.QuickCheck ()
import           Test.QuickCheck.Instances ()

main :: IO ()
main = doctest =<< glob "./library"

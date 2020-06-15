-- |

module Data.Error.Types
  ( NotFound   (..)
  , NotAllowed (..)
  ) where

data NotFound entity
  = NotFound
  deriving (Show, Eq)

data NotAllowed entity user
  = NotAllowed entity user
  deriving (Show, Eq)

-- FIXME and more!

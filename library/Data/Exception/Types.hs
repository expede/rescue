-- | Common exceptions
module Data.Exception.Types
  ( NotFound      (..)
  , NotAllowed    (..)
  , AlreadyExists (..)
  , OutOfBounds   (..)
  , DivideByZero  (..)
  ) where

import           Data.Text

data NotFound entity
  = NotFound
  deriving (Show, Eq)

data NotAllowed entity user
  = NotAllowed entity user
  deriving (Show, Eq)

data AlreadyExists entity
  = AlreadyExists entity
  deriving (Show, Eq)

data OutOfBounds entity index
  = OutOfBounds index
  deriving (Show, Eq)

data DivideByZero
  = DivideByZero
  deriving (Show, Eq)

data InvalidFormat entity
  = InvalidFormat entity
  deriving (Show, Eq)

-- | Attach a message to an exception, typicaly for runtime user feedback
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedStrings
-- >>> show $ InvalidFormat "foo" `WithMessage` "Not a valid JSON object"
-- "InvalidFormat \"foo\" `WithMessage` \"Not a valid JSON object\""
data WithMessage err
  = err `WithMessage` Text
  deriving (Show, Eq)

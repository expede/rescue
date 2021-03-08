-- | Common exceptions
module Data.Exception.Types
  ( AlreadyExists (..)
  , DivideByZero  (..)
  , InvalidFormat (..)
  , NotAllowed    (..)
  , NotFound      (..)
  , OutOfBounds   (..)
  , WithMessage   (..)
  ) where

import           Data.Text

-- | Entity not found
data NotFound entity
  = NotFound
  deriving (Show, Eq)

-- | Action not allowed by user
data NotAllowed user entity =
  NotAllowed
    { user   :: !user
    , entity :: !entity
    }
  deriving (Show, Eq)

-- | Requested entity already exists; a conflict
newtype AlreadyExists entity
  = AlreadyExists entity
  deriving (Show, Eq)

instance Functor AlreadyExists where
  fmap f (AlreadyExists entity') = AlreadyExists $ f entity'

-- | Requested index is out of bounds
newtype OutOfBounds entity index
  = OutOfBounds index
  deriving (Show, Eq)

instance Functor (OutOfBounds entity) where
  fmap f (OutOfBounds index') = OutOfBounds $ f index'

-- | Arithmetic divide by zero error
data DivideByZero
  = DivideByZero
  deriving (Show, Eq)

-- | Invalid format for entity (e.g. bad JSON)
newtype InvalidFormat entity
  = InvalidFormat entity
  deriving (Show, Eq)

instance Functor InvalidFormat where
  fmap f (InvalidFormat entity') = InvalidFormat $ f entity'

-- | Attach a message to an exception, typicaly for runtime user feedback
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedStrings
-- >>> show $ InvalidFormat "foo" `WithMessage` "Not a valid JSON object"
-- "InvalidFormat \"foo\" `WithMessage` \"Not a valid JSON object\""
data WithMessage err
  = !err `WithMessage` !Text
  deriving (Show, Eq)

instance Functor WithMessage where
  fmap f (WithMessage err msg) = WithMessage (f err) msg

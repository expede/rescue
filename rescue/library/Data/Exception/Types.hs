{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Common exceptions
module Data.Exception.Types
  ( NotFound      (..)
  , NotAllowed    (..)
  , AlreadyExists (..)
  , OutOfBounds   (..)
  , DivideByZero  (..)
  ) where

import           Control.Exception
import           Data.Text
import           Type.Reflection

data NotFound entity
  = NotFound
  deriving (Show, Eq)

instance (Typeable entity, Show entity) => Exception (NotFound entity)

data NotAllowed entity user
  = NotAllowed entity user
  deriving (Show, Eq)

instance
  ( Typeable entity
  , Typeable user
  , Show entity
  , Show user
  ) => Exception (NotAllowed entity user)

newtype AlreadyExists entity
  = AlreadyExists entity
  deriving (Show, Eq, Exception)

newtype OutOfBounds entity index
  = OutOfBounds index
  deriving (Show, Eq, Exception)

data DivideByZero
  = DivideByZero
  deriving (Show, Eq)

instance Exception DivideByZero

newtype InvalidFormat entity
  = InvalidFormat entity
  deriving (Show, Eq, Exception)

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

instance Exception err => Exception (WithMessage err)

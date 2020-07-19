{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Common exceptions
module Data.Exception.Types
  ( NotFound      (..)
  , NotAllowed    (..)
  , AlreadyExists (..)
  , OutOfBounds   (..)
  , DivideByZero  (..)
  , InvalidFormat (..)
  -- * Reexports
  , module Data.Exception.Message.Types
  ) where

import           Control.Exception
import           Type.Reflection

import           Data.Exception.Message       as Exception
import           Data.Exception.Message.Types

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

instance Exception.Message DivideByZero where
  publicMsg _ = "Division by zero is not meaningful in this context"

newtype InvalidFormat entity
  = InvalidFormat entity
  deriving (Show, Eq, Exception)

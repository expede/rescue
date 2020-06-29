{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Control.Monad.Cleanup.Class (MonadCleanup (..)) where

import           Control.Exception
import           Control.Monad.Rescue

import           Data.WorldPeace

-- | Safely work with resources when an asynchronous exception may be thrown
class (Raises m SomeException, MonadRescue m) => MonadCleanup m where
  cleanup
    :: m resource                                   -- ^ Acquire some resource
    -> (resource -> OpenUnion (Errors m) -> m _ig1) -- ^ Cleanup and re-raise
    -> (resource ->                         m _ig2) -- ^ Cleanup normally
    -> (resource ->                         m a)    -- ^ Inner action to perform with the resource
    -> m a

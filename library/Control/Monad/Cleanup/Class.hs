{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Control.Monad.Cleanup.Class (MonadCleanup (..)) where

import           Control.Exception
import           Control.Monad.Rescue

-- | Safely work with resources when an asynchronous exception may be thrown
class (m `Raises` SomeException, MonadRescueFrom m m) => MonadCleanup m where
  cleanup
    :: m resource                          -- ^ Acquire some resource
    -> (resource -> ErrorCase m -> m _ig1) -- ^ Cleanup and re-raise
    -> (resource ->                m _ig2) -- ^ Cleanup normally
    -> (resource ->                m a)    -- ^ Inner action to perform with the resource
    -> m a

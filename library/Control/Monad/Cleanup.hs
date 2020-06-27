{-# LANGUAGE LambdaCase #-}

-- | Cleanly release resources and clean up contexts

module Control.Monad.Cleanup
  ( cleanRetry
  , always
  -- * Reexport
  , module Control.Monad.Rescue
  , module Control.Monad.Cleanup.Class
  ) where

import           Control.Monad.Cleanup.Class
import           Control.Monad.Rescue

import           Numeric.Natural

-- | Equivalent of 'finally'
always :: MonadCleanup m => m a -> m b -> m a
always action finalizer =
  cleanup (pure ())
          (\_ _ -> finalizer)
          (\_   -> finalizer)
          (\_   -> action)

cleanRetry :: MonadCleanup m => Natural -> m a -> m a
cleanRetry 0     action = action
cleanRetry times action =
  cleanup (pure ())
          (\_ _ -> cleanRetry (times - 1) action)
          (\_   -> pure ())
          (\_   -> action)

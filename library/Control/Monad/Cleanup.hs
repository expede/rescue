{-# LANGUAGE LambdaCase #-}

-- | Cleanly release resources and clean up contexts

module Control.Monad.Cleanup
  ( retry
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

retry :: MonadCleanup m => Natural -> m a -> m a
retry 0     action = action
retry times action =
  cleanup (pure ())
          (\_ _ -> retry (times - 1) action)
          (\_   -> pure ())
          (\_   -> action)

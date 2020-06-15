{-# LANGUAGE LambdaCase #-}

-- |

module Control.Monad.Cleanup
  ( cleanRetry
  , lastly
  -- * Reexport
  , module Control.Monad.Rescue
  , module Control.Monad.Cleanup.Class
  ) where

import           Control.Monad.Cleanup.Class
import           Control.Monad.Rescue

import           Numeric.Natural

-- | Equivalent of 'finally'
lastly :: MonadCleanup m => m a -> m b -> m a
lastly action finalizer =
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

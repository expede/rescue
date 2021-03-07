module Control.Monad.Trans.Error
  ( mapErrorT
  -- * Reexports
  , module Control.Monad.Trans.Error.Class
  ) where

import           Control.Monad.Raise
import           Control.Monad.Trans.Error.Class

import           Data.WorldPeace

mapErrorT
  :: ( MonadTransError t sourceErrs m
     , MonadRaise     (t targetErrs m)
     , CheckErrors    (t targetErrs m)
     )
  => (OpenUnion (Errors (t sourceErrs m)) -> OpenUnion (Errors (t targetErrs m)))
  -> t sourceErrs m a
  -> t targetErrs m a
mapErrorT f = onRaise (raise . f)

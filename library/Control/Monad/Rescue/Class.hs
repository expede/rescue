{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | 

module Control.Monad.Rescue.Class (MonadRescue (..)) where

import           Control.Monad.Raise
-- import           Control.Monad.Trans.Except

import           Data.WorldPeace

class MonadRaise errs m => MonadRescue errs m where
  try :: m a -> m (Either (OpenUnion errs) a)

instance MonadRescue errs (Either (OpenUnion errs)) where
  try = Right

-- instance Monad m => MonadRescue errs (ExceptT (OpenUnion errs) m) where
--   try (ExceptT action) = ExceptT (try <$> action)

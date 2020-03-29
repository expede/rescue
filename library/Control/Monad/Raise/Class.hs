{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | 

module Control.Monad.Raise.Class (MonadRaise (..)) where

import           Data.WorldPeace
import           Control.Monad.Trans.Except

class Monad m => MonadRaise errs m where
  raise' :: OpenUnion errs -> m a

instance MonadRaise errs [] where
  raise' _ = []

instance MonadRaise errs Maybe where
  raise' _ = Nothing

instance MonadRaise errs (Either (OpenUnion errs)) where
  raise' = Left

instance Monad m => MonadRaise errs (ExceptT (OpenUnion errs) m) where
  raise' = ExceptT . pure . Left

-- instance MonadRaise errs IO where
--   raise' = raiseIO#

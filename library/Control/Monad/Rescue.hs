{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Control.Monad.Rescue
  ( MonadRescue (..)
  , rescue
  , raise
  , reraise
  , handle
  ) where

import           Control.Monad.Trans.Except
import           Control.Monad.Raise

import           Data.WorldPeace

class MonadRaise errs m => MonadRescue errs m where
  expose :: m a -> m (Either (OpenUnion errs) a)

instance MonadRescue errs (Either (OpenUnion errs)) where
  expose = Right
  {-# INLINE expose #-}

instance Monad m => MonadRescue errs (ExceptT (OpenUnion errs) m) where
  expose (ExceptT action) = ExceptT (expose <$> action)

rescue :: MonadRescue errs m => m a -> (Either (OpenUnion errs) a -> m b) -> m b
rescue action handler = expose action >>= handler

reraise :: forall innerErrs outerErrs m a .
  ( Contains    innerErrs  outerErrs
  , MonadRescue innerErrs            m
  , MonadRaise             outerErrs m
  )
  => m a
  -> m a
reraise action = rescue action handler
  where
    handler :: Either (OpenUnion innerErrs) a -> m a
    handler = \case
      Left  err -> raise' (relaxOpenUnion err :: OpenUnion outerErrs)
      Right val  -> return val

handle :: MonadRescue errs m => m a -> (OpenUnion errs -> m a) -> m a
handle action handler = expose action >>= either handler pure

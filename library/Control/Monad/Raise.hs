{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- | Monadic raise semantics

module Control.Monad.Raise
  ( MonadRaise  (..)
  , raise
  , ensure
  ) where

import           Data.OpenUnion.Class
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

raise :: forall m err errs a .
  ( ToOpenUnion err errs
  , MonadRaise      errs m
  )
  => err
  -> m a
raise err = raise' (toOpenUnion err :: OpenUnion errs)

ensure :: forall err errs m a .
  ( ToOpenUnion err errs
  , MonadRaise errs m
  )
  => Either err a
  -> m a
ensure = \case
  Left err  -> raise' (toOpenUnion err :: OpenUnion errs)
  Right val -> pure val

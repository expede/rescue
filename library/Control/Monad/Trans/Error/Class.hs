{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Trans.Error.Class (MonadTransError (..)) where

import           Control.Monad.Raise.Class
import           Control.Monad.Trans.Class

import           Data.Kind
import           Data.WorldPeace

class MonadTrans (t sourceErrs) => MonadTransError t (sourceErrs :: [Type]) m where
  onRaise
    :: (OpenUnion (Errors (t sourceErrs m)) -> t targetErrs m a)
    -> t sourceErrs m a
    -> t targetErrs m a

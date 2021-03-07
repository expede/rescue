{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Control.Monad.Trans.Error.Class (MonadTransError (..)) where

-- -- FIXME Reinterpret?
-- class ErrorFunctor m sourceErrs targetErrs where
--   mapErrors :: _
--
-- class Handlable m sourceErrs where
--   handley :: _

import           Control.Monad.Raise.Class
import           Control.Monad.Trans.Class

import           Data.Kind

import           Data.WorldPeace

class MonadTrans (t (Errors m)) => MonadTransError (t :: [Type] -> (Type -> Type) -> Type -> Type) m where
  mappy' :: (OpenUnion (Errors (t sourceErrs m)) -> OpenUnion (Errors (t targetErrs m)))
        -> t sourceErrs m a
        -> t targetErrs m a


-- FIXME add standard instances

-- FIXME probably move module, since Control.Monad.Trans.Class

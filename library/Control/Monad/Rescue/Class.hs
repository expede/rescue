{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Monad.Rescue.Class (MonadRescue (..)) where

import           Control.Monad.Trans.Except

import           Data.WorldPeace

class Monad m => MonadRescue t m where
  rescue ::
    ( innerErrs ~ Remove err outerErrs
    , ElemRemove err outerErrs
    )
    => (err -> t (OpenUnion innerErrs) m a)
    -> t (OpenUnion outerErrs) m a
    -> t (OpenUnion innerErrs) m a

instance Monad m => MonadRescue ExceptT m where
  rescue handler (ExceptT action) =
    ExceptT $
      action >>= \case
        Left outerErrs -> openUnionHandle (return . Left) (runExceptT . handler) outerErrs
        Right val      -> return $ Right val

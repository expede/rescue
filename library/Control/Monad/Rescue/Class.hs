{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Monad.Rescue.Class (MonadRescue (..)) where

import           Data.WorldPeace

class Monad m => MonadRescue t m where
  rescue ::
    ( innerErrs ~ Remove err outerErrs
    , ElemRemove err outerErrs
    )
    => (err -> t innerErrs m a)
    -> t outerErrs m a
    -> t innerErrs m a


{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME

module Control.Monad.Reraise.Class -- FIXME rename
  ( MonadRelaxErrors (..)
  ) where

import           Control.Monad.Cont
import           Data.WorldPeace

-- FIXME tests

class (Monad n, Monad m) => MonadRelaxErrors n m where
  relaxErrors :: n a -> m a -- FIXME perhaps reraise, relaxErrors, or ajust, relaxErrorsErrs, or something?
  -- NOTE / FIXME was recontextualize

  -- For docs: movng up in a module herarchy, we tend to relax the erorr cntext to allow more potential errors

instance Monad m => MonadRelaxErrors m m where
  relaxErrors action = action

instance MonadRelaxErrors Maybe [] where
  relaxErrors Nothing  = []
  relaxErrors (Just x) = [x]

instance MonadRelaxErrors (Either errs) Maybe where
  relaxErrors (Left  _)   = Nothing
  relaxErrors (Right val) = Just val

instance MonadRelaxErrors (Either errs) [] where
  relaxErrors (Left  _)   = []
  relaxErrors (Right val) = [val]

instance Contains inner outer => MonadRelaxErrors (Either (OpenUnion inner)) (Either (OpenUnion outer)) where
  relaxErrors (Left err)  = Left $ relaxOpenUnion err
  relaxErrors (Right val) = Right val

instance (Monad m, Monad n, MonadTrans t, Monad (t m), MonadRelaxErrors n m) => MonadRelaxErrors n (t m) where
  relaxErrors = lift . relaxErrors

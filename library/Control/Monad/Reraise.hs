{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | FIXME

module Control.Monad.Reraise where

import           Control.Monad.Cont
import           Data.WorldPeace

import           Control.Monad.Foo
import           Control.Monad.Raise

class MonadReraise n m where
  recontextualize :: n a -> m a -- FIXME reraise
 
instance MonadReraise m m where
  recontextualize action = action

instance MonadReraise Maybe [] where
  recontextualize Nothing  = []
  recontextualize (Just x) = [x]

instance MonadReraise (Either errs) Maybe where
  recontextualize (Left  _)   = Nothing
  recontextualize (Right val) = Just val

instance MonadReraise (Either errs) [] where
  recontextualize (Left  _)   = []
  recontextualize (Right val) = [val]

instance ToOpenUnion inner outer => MonadReraise (Either inner) (Either (OpenUnion outer)) where
  recontextualize (Left err)  = raise err
  recontextualize (Right val) = pure val

instance (Monad m, MonadTrans t, MonadReraise n m) => MonadReraise n (t m) where
  recontextualize = lift . recontextualize

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

instance Contains inner outer => MonadReraise (Either (OpenUnion inner)) (Either (OpenUnion outer)) where
  recontextualize (Left err)  = Left $ relaxOpenUnion err
  recontextualize (Right val) = Right val

instance (Monad m, MonadTrans t, MonadReraise n m) => MonadReraise n (t m) where
  recontextualize = lift . recontextualize

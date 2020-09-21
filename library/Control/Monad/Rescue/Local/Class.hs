{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- |

module Control.Monad.Rescue.Local.Class where

import           Data.WorldPeace

import           Control.Monad.Raise.Class
import           Control.Monad.Rescue.Class

-- FIXME may actully be a better base instance for MnadResce iteslef
class MonadRescue m => LocalExceptions m n where
  runLocalExceptions :: m a -> n (Either (OpenUnion (Errors m)) a) -- FIXME shrthand for OpenUnion errors m

instance MonadRescue m => LocalExceptions m m where
  runLocalExceptions action = attempt action

instance Monad m => LocalExceptions Maybe m where
  runLocalExceptions Nothing  = pure . Left $ openUnionLift ()
  runLocalExceptions (Just a) = pure $ Right a

instance Monad m => LocalExceptions (Either (OpenUnion errs)) m where
  runLocalExceptions action = pure action

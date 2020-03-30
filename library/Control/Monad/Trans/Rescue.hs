{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |

module Control.Monad.Trans.Rescue
  ( RescueT (..)
  , Rescue
  ) where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Fix

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class

import           Data.Functor.Identity
import           Data.Proxy
import           Data.WorldPeace

newtype RescueT errs m a = RescueT { runRescueT :: m (Either (OpenUnion errs) a) }

type Rescue errs = RescueT errs Identity

instance Functor m => Functor (RescueT errs m) where
  fmap f (RescueT inner) = RescueT $ fmap (fmap f) inner

instance Applicative m => Applicative (RescueT errs m) where
  pure = RescueT . pure . pure
  (RescueT fs) <*> (RescueT xs) = RescueT $ do
    innerFs <- fs
    innerXs <- xs
    return(innerFs <*> innerXs)

instance Monad m => Monad (RescueT errs m) where
  RescueT action >>= k = RescueT $ action >>= \case
    Left  err -> return (Left err)
    Right val -> runRescueT (k val)

instance MonadTrans (RescueT errs) where
  lift action = RescueT (Right <$> action)

instance MonadIO m => MonadIO (RescueT errs m) where
  liftIO io = RescueT $ do
    action <- liftIO io
    return (Right action)

instance MonadFix m => MonadFix (RescueT errs m) where
  mfix f = RescueT . mfix $ \a ->
    runRescueT . f $ case a of
       Right r -> r
       _       -> error "Empty mfix argument" -- absurd

instance Foldable m => Foldable (RescueT errs m) where
  foldMap f (RescueT m) = foldMap (foldMapEither f) m where
    foldMapEither g (Right a) = g a
    foldMapEither _ (Left _) = mempty

instance (Monad m, Traversable m) => Traversable (RescueT errs m) where
  traverse f (RescueT m) = RescueT <$> traverse (traverseEither f) m
    where
      traverseEither g (Right val) = Right <$> g val
      traverseEither _ (Left  err) = pure (Left err)

instance Monad m => MonadRaise errs (RescueT errs m) where
  raise' _ = RescueT . pure . Left

instance Monad m => MonadRescue errs (RescueT errs m) where
  try' _ (RescueT action) = RescueT $ fmap Right action

instance forall errs m .
  (IsMember SomeException errs, Monad m) => MonadThrow (RescueT errs m) where
    throwM err = raiseAs (Proxy @errs) (toException err)

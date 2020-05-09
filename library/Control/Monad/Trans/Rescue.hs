{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module supplies a "pure" monad transformer that
--   can be used for adding 'MonadRescue' behaviour to a transformer stack

module Control.Monad.Trans.Rescue
  ( RescueT (..)
  , Rescue
  , runRescue
  ) where

import           Control.Monad.Catch hiding (try) -- FIXME: rename try with catch or attempt or something
import           Control.Monad.Cont
import           Control.Monad.Fix

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class

import           Data.Functor.Identity
import           Data.Proxy
import           Data.WorldPeace




import Control.Monad.Foo

-- | Add type-directed error handling abilities to a 'Monad'.
newtype RescueT errs m a
  = RescueT { runRescueT :: m (Either (OpenUnion errs) a) }

-- | A specialized version of 'RescueT'.
type Rescue errs = RescueT errs Identity

runRescue :: Rescue errs a -> Either (OpenUnion errs) a
runRescue = runIdentity . runRescueT

instance Eq (m (Either (OpenUnion errs) a)) => Eq (RescueT errs m a) where
  RescueT a == RescueT b = a == b

instance Show (m (Either (OpenUnion errs) a)) => Show (RescueT errs m a) where
  show (RescueT inner) = "RescueT (" <> show inner <> ")"

instance Functor m => Functor (RescueT errs m) where
  fmap f (RescueT inner) = RescueT $ fmap (fmap f) inner

instance Applicative m => Applicative (RescueT errs m) where
  pure = RescueT . pure . pure
  (RescueT fs) <*> (RescueT xs) = RescueT $ do
    innerFs <- fs
    innerXs <- xs
    return (innerFs <*> innerXs)

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

-- instance (Monad m, ToOpenUnion inner outer, OpenUnion outer ~ OpenUnion errs) => MonadRaise err (RescueT errs m) where
instance (Monad m, ToOpenUnion err errs) => MonadRaise err (RescueT errs m) where
  raise  = RescueT . pure . Left . consistent

-- instance
--   ( Monad m
--   , ToOpenUnion err errs
--   )
--   => MonadRescue err (RescueT errs m) where
--     try (RescueT action) = RescueT $ fmap Right action

-- FIXME
-- instance forall errs m .
--   ( IsMember SomeException errs
--   , Monad m
--   )
--   => MonadThrow (RescueT errs m) where
--     throwM err = raise @errs (toException err)

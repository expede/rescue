{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME

module Control.Monad.Trans.Rescue.Types
  ( RescueT (..)
  , Rescue
  , runRescue
  ) where

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Fix
import           Control.Monad.Rescue

import           Data.Functor.Identity
import           Data.WorldPeace

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

instance (IsMember err errs, Monad m) => MonadRaise err (RescueT errs m) where
  raise err = RescueT . pure . Left $ openUnionLift err

instance Monad m => MonadRescue errs (RescueT errs m) where
  attempt (RescueT action) = RescueT (Right <$> action)

instance (IsMember SomeException errs, Monad m) => MonadThrow (RescueT errs m) where
  throwM = RescueT . pure . Left . openUnionLift . toException

-- -- instance forall errs err m . (MonadRescue errs (RescueT errs m), IsMember SomeException errs, IsMember err errs, Monad m) => MonadCatch (RescueT errs m) where
-- instance forall err errs m . (Exception err, MonadRescue errs (RescueT errs m), IsMember err errs, IsMember SomeException errs, Monad m) => MonadCatch (RescueT errs m) where
--   catch action handler = -- (handler :: err -> RescueT errs m a) =
--     attempt action >>= \case
--       Right val  -> pure val
--       Left  errs ->
--         case openUnionMatch errs of -- openUnionHandle raise withSomeException errs
--           Nothing -> undefined -- raise errs
--           Just exc -> withSomeException exc

--     where
--       withSomeException :: SomeException -> RescueT errs m a
--       withSomeException err =
--         case fromException err of
--           Nothing   -> raise err
--           Just err' -> handler err'

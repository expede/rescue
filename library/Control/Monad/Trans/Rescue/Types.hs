{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The 'RescueT' transformer
module Control.Monad.Trans.Rescue.Types
  ( RescueT (..)
  , Rescue
  , runRescue
  ) where

import Prelude

import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Fix
import           Control.Monad.Reader
import           Control.Monad.Rescue
import           Control.Monad.Base

import           Data.Functor.Identity
import           Data.WorldPeace

-- | Add type-directed error handling abilities to a 'Monad'
newtype RescueT errs m a
  = RescueT { runRescueT :: m (Either (OpenUnion errs) a) }

-- | A specialized version of 'RescueT' to be used without a transfromer stack
type Rescue errs = RescueT errs Identity

runRescue :: Rescue errs a -> Either (OpenUnion errs) a
runRescue = runIdentity . runRescueT

mapRescueT
  :: (  n (Either (OpenUnion errs)  a)
     -> m (Either (OpenUnion errs') b)
     )
  -> RescueT errs  n a
  -> RescueT errs' m b
mapRescueT f (RescueT n) = RescueT $ f n

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

instance MonadBase b m => MonadBase b (RescueT errs m) where
  liftBase = liftBaseDefault

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
    foldMapEither _ (Left _)  = mempty

instance (Monad m, Traversable m) => Traversable (RescueT errs m) where
  traverse f (RescueT m) = RescueT <$> traverse (traverseEither f) m
    where
      traverseEither g (Right val) = Right <$> g val
      traverseEither _ (Left  err) = pure (Left err)

instance Monad m => MonadRaise (RescueT errs m) where
  type Errors (RescueT errs m) = errs
  raise err = RescueT . pure $ raise err

instance
  ( Monad       m
  , MonadBase n m
  )
  => MonadRescueFrom (RescueT errs n) m where
    attempt (RescueT action) = liftBase action

instance MonadThrow m => MonadThrow (RescueT errs m) where
  throwM = lift . throwM

instance MonadCatch m => MonadCatch (RescueT errs m) where
  catch (RescueT m) f = RescueT $ catch m (runRescueT . f)

instance MonadReader cfg m => MonadReader cfg (RescueT errs m) where
  ask    = lift ask
  local  = mapRescueT . local
  reader = lift . reader

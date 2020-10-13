{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The 'CleanupT' transformer for adding async exceptions to a stack

module Control.Monad.Trans.Cleanup.Types
  ( CleanupT (..)
  , CleanupIO
  ) where

import           Control.Applicative
import           Control.Monad

import           Control.Monad.Base
import           Control.Monad.Catch        as Catch
import           Control.Monad.Cleanup
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Data.Functor
import           Data.Functor.Contravariant

import           Data.WorldPeace

-- | Adds 'SomeException' to an exception stack,
--   and thus /aware/ of async exceptions
newtype CleanupT m a = CleanupT { runCleanupT :: m a }

type CleanupIO a = CleanupT IO a

instance Eq (m a) => Eq (CleanupT m a) where
  CleanupT a == CleanupT b = a == b

instance Show (m a) => Show (CleanupT m a) where
  show (CleanupT x) = "CleanupT (" <> show x <> ")"

instance Functor m => Functor (CleanupT m) where
  fmap f (CleanupT action) = CleanupT (fmap f action)

instance Contravariant f => Contravariant (CleanupT f) where
  contramap f = CleanupT . contramap f . runCleanupT

instance Foldable t => Foldable (CleanupT t) where
  foldMap f (CleanupT a) = foldMap f a
  foldr f z (CleanupT a) = foldr f z a

instance Traversable t => Traversable (CleanupT t) where
  traverse f (CleanupT a) = CleanupT <$> traverse f a

instance Applicative f => Applicative (CleanupT f) where
  pure = CleanupT . pure
  CleanupT f <*> CleanupT x = CleanupT (f <*> x)

instance Alternative f => Alternative (CleanupT f) where
  empty = CleanupT empty
  CleanupT a <|> CleanupT b  = CleanupT (a <|> b)

instance Monad m => Monad (CleanupT m) where
  CleanupT x >>= f = CleanupT (runCleanupT . f =<< x)

instance MonadTrans CleanupT where
  lift = CleanupT

instance MonadIO m => MonadIO (CleanupT m) where
  liftIO = CleanupT . liftIO

instance MonadPlus m => MonadPlus (CleanupT m) where
  mzero = CleanupT mzero
  mplus (CleanupT a) (CleanupT b) = CleanupT (mplus a b)

instance MonadFix m => MonadFix (CleanupT m) where
  mfix f = CleanupT (mfix (runCleanupT . f))

instance MonadThrow m => MonadThrow (CleanupT m) where
  throwM = CleanupT . throwM

instance MonadCatch m => MonadCatch (CleanupT m) where
  catch (CleanupT action) handler =
    CleanupT $ catch action (runCleanupT . handler)

instance MonadMask m => MonadMask (CleanupT m) where
   mask action = CleanupT $ mask (\u -> runCleanupT (action $ q u))
    where
      q :: (m a -> m a) -> CleanupT m a -> CleanupT m a
      q u = CleanupT . u . runCleanupT

   uninterruptibleMask a =
    CleanupT $ uninterruptibleMask (\u -> runCleanupT (a $ q u))
      where
        q :: (m a -> m a) -> CleanupT m a -> CleanupT m a
        q u = CleanupT . u . runCleanupT

   generalBracket acquire release use = CleanupT $
     generalBracket
       (runCleanupT acquire)
       (\resource exitCase -> runCleanupT (release resource exitCase))
       (runCleanupT . use)

instance
  ( Contains (Errors m) (Errors m)
  , MonadRaise m
  , MonadThrow m
  )
  => MonadRaise (CleanupT m) where
  type Errors (CleanupT m) = SomeException ': Errors m

  raise err = openUnion raiser throwM errsUnion
    where
      errsUnion :: OpenUnion (SomeException ': Errors m)
      errsUnion = include err

      raiser :: Contains err (Errors m) => OpenUnion err -> CleanupT m a
      raiser = CleanupT . raise

instance MonadBase m m => MonadBase m (CleanupT m) where
  liftBase = liftBaseDefault

instance (Monad m, MonadRescue m) => MonadRescueFrom m (CleanupT m) where
  attempt = CleanupT . attempt

instance forall n m .
  ( MonadRescueFrom n m
  , MonadBase       n m
  , MonadRescue     n
  , MonadCatch      n
  , Contains (Errors n) (Errors n)
  , Contains (Errors n) (SomeException ': Errors n)
  )
  => MonadRescueFrom (CleanupT n) m where
    attempt (CleanupT action) =
      liftBase $
        inner <&> \case
          Left err          -> Left $ include err
          Right (Left  err) -> Left $ include err
          Right (Right val) -> Right val
      where
        inner =
          Catch.try action >>= \case
            Left  e@(SomeException _) -> return $ Left e
            Right (val :: a)          -> Right <$> attempt (pure val :: n a)

instance
  ( Contains (Errors m) (Errors m)
  , Contains (Errors m) (SomeException ': Errors m)
  , MonadBase m m
  , MonadRescue m
  , MonadMask   m
  )
  => MonadCleanup (CleanupT m) where
  cleanup acquire onErr onOk action =
    mask $ \restore -> do
      resource <- acquire

      attempt (restore $ action resource) >>= \case
        Left errs -> do
          _ <- uninterruptibleMask_ $
                 fmap (\_ -> ()) (onErr resource errs)
                   `catch` \(_ :: SomeException) -> return ()

          raise errs

        Right output -> do
          _ <- onOk resource
          return output

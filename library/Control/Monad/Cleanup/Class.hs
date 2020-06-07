{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | FIXME

module Control.Monad.Cleanup.Class where

import           Data.Functor

import Control.Monad.Catch

import           GHC.Base

import Exception hiding (catch, mask, uninterruptibleMask, uninterruptibleMask_)

import           Control.Monad.Rescue
import           Data.WorldPeace

-- | Safely work with resources when an asynchronous exception may be thrown
class (Raises SomeException m, MonadRescue m) => MonadCleanup m where
  cleanup
    :: m resource                                   -- ^ Acquire some resource
    -> (resource -> OpenUnion (Errors m) -> m _ig1) -- ^ Cleanup and re-raise
    -> (resource ->                         m _ig2) -- ^ Cleanup normally
    -> (resource ->                         m a)    -- ^ Inner action to perform with the resource
    -> m a

-- Adds SomeException to an Error stack
newtype AsyncAwareT m a = AsyncAwareT { unAwareT :: m a }

instance Functor m => Functor (AsyncAwareT m) where
  fmap f (AsyncAwareT action) = AsyncAwareT (fmap f action)
  {-# INLINE fmap #-}

-- instance Foldable
-- instance Traversable
-- instance MonadTrans
-- instance Alternative
-- instance Eq, Show, Display

instance Applicative m => Applicative (AsyncAwareT m) where
  pure = AsyncAwareT . pure
  {-# INLINE pure #-}

  AsyncAwareT f <*> AsyncAwareT x = AsyncAwareT (f <*> x)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (AsyncAwareT m) where
  AsyncAwareT x >>= f = AsyncAwareT (x >>= unAwareT . f)
  {-# INLINE (>>=) #-}

instance
  -- ( Subset (OpenUnion (Errors m)) (OpenUnion (Errors (AsyncAwareT m)))
  ( MonadRaise m
  )
  => MonadRaise (AsyncAwareT m) where
  type Errors (AsyncAwareT m) = SomeException ': Errors m
  raise = AsyncAwareT . raise -- FIXME brooke, you're working on this bit -- type constraint issue here

type AsyncAwareIO a = AsyncAwareT IO a

instance MonadRescue (AsyncAwareT IO) where
  attempt (AsyncAwareT action) = AsyncAwareT $
    tryIO action <&> \case
      Right val -> Right val
      Left  err  -> Left $ include err

instance MonadThrow m => MonadThrow (AsyncAwareT m) where
  throwM = AsyncAwareT . throwM

instance MonadCatch m => MonadCatch (AsyncAwareT m) where
  catch (AsyncAwareT action) handler =
    AsyncAwareT $ catch action (unAwareT . handler)

instance MonadMask m => MonadMask (AsyncAwareT m) where
   mask a = AsyncAwareT $ mask $ \u -> unAwareT (a $ q u)
    where q :: (m a -> m a) -> AsyncAwareT m a -> AsyncAwareT m a
          q u = AsyncAwareT . u . unAwareT

   uninterruptibleMask a =
    AsyncAwareT $ uninterruptibleMask $ \u -> unAwareT (a $ q u)
      where q :: (m a -> m a) -> AsyncAwareT m a -> AsyncAwareT m a
            q u = AsyncAwareT . u . unAwareT

   generalBracket acquire release use = AsyncAwareT $
     generalBracket
       (unAwareT acquire)
       (\resource exitCase -> unAwareT (release resource exitCase))
       (\resource -> unAwareT (use resource))

instance MonadCleanup (AsyncAwareT IO) where
  cleanup acquire onErr onOk action =
    mask $ \restore -> do
      resource <- acquire
 
      -- NOTE MonadRescue AsyncIO catches all throwIOs
      attempt (restore $ action resource) >>= \case
        Left errs -> do
          _ <- uninterruptibleMask_ $
             fmap (\_ -> ()) (onErr resource errs)
                `catch` \(_ :: SomeException) -> return ()

          raise errs -- NOTE `raise` *is* throwIO without the `toException`

        Right output -> do
          _ <- onOk resource
          return output
 
-- finally :: MonadCleanup m => m a -> m b -> m a

-- cleanRetry :: MonadCleanup m => Nat -> m a -> m a

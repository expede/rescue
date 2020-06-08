{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | FIXME add docs

module Control.Monad.Cleanup.Class (MonadCleanup (..)) where

import           Control.Monad.Catch  as Catch
import           Control.Monad.Rescue

import           Data.WorldPeace

-- FIXME move somewhere better and rename

type AllErrs m = OpenUnion (Errors m)

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

instance MonadThrow m => MonadThrow (AsyncAwareT m) where
  throwM = AsyncAwareT . throwM

instance
  ( Contains (Errors m) (Errors m)
  , MonadRaise m
  , MonadThrow m
  )
  => MonadRaise (AsyncAwareT m) where
  type Errors (AsyncAwareT m) = SomeException ': Errors m

  raise err = openUnion raiser throwM errsUnion
    where
      errsUnion :: OpenUnion (SomeException ': Errors m)
      errsUnion = include err

      raiser :: Contains err (Errors m) => OpenUnion err -> AsyncAwareT m a
      raiser = AsyncAwareT . raise

instance
  ( Contains (Errors m) (Errors m)
  , MonadCatch  m
  , MonadRescue m
  )
  => MonadRescue (AsyncAwareT m) where -- TODO needs testing to check that this works as intended
  attempt action =
    Catch.try action >>= \case
      Left  e@(SomeException _) -> return . Left $ include e
      Right result              -> attempt $ pure result

type AsyncAwareIO a = AsyncAwareT IO a

instance MonadCatch m => MonadCatch (AsyncAwareT m) where
  catch (AsyncAwareT action) handler =
    AsyncAwareT $ catch action (unAwareT . handler)

instance MonadMask m => MonadMask (AsyncAwareT m) where
   mask a = AsyncAwareT $ mask $ \u -> unAwareT (a $ q u)
    where
      q :: (m a -> m a) -> AsyncAwareT m a -> AsyncAwareT m a
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

instance
  ( Contains (Errors m) (Errors m)
  , Contains (Errors m) (SomeException ': Errors m)
  , MonadRescue m
  , MonadMask   m
  )
  => MonadCleanup (AsyncAwareT m) where
  cleanup acquire onErr onOk action =
    mask $ \restore -> do
      resource <- acquire

      attempt (restore $ action resource) >>= \case
        Left errs -> do
          _ <- uninterruptibleMask_$
             fmap (\_ -> ()) (onErr resource errs)
                `catch` \(_ :: SomeException) -> return ()

          raise errs

        Right output -> do
          _ <- onOk resource
          return output

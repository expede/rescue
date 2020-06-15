{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | FIXME docs

-- FIXME perhaps CleanupT?

module Control.Monad.Trans.Cleanup.Types
  ( CleanupT (..)
  , CleanupIO
  ) where

import           Control.Monad.Catch   as Catch
import           Control.Monad.Cleanup

import           Data.WorldPeace

-- | Adds 'SomeException' to an Error stack
newtype CleanupT m a = CleanupT { unAwareT :: m a }

type CleanupIO a = CleanupT IO a

instance Functor m => Functor (CleanupT m) where
  fmap f (CleanupT action) = CleanupT (fmap f action)
  {-# INLINE fmap #-}

-- instance Foldable
-- instance Traversable
-- instance MonadTrans
-- instance Alternative
-- instance Eq, Show, Display

instance Applicative m => Applicative (CleanupT m) where
  pure = CleanupT . pure
  {-# INLINE pure #-}

  CleanupT f <*> CleanupT x = CleanupT (f <*> x)
  {-# INLINE (<*>) #-}

instance Monad m => Monad (CleanupT m) where
  CleanupT x >>= f = CleanupT (x >>= unAwareT . f)
  {-# INLINE (>>=) #-}

instance MonadThrow m => MonadThrow (CleanupT m) where
  throwM = CleanupT . throwM

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

instance
  ( Contains (Errors m) (Errors m)
  , MonadCatch  m
  , MonadRescue m
  )
  => MonadRescue (CleanupT m) where -- TODO needs testing to check that this works as intended
  attempt action =
    Catch.try action >>= \case
      Left  e@(SomeException _) -> return . Left $ include e
      Right result              -> attempt $ pure result


instance MonadCatch m => MonadCatch (CleanupT m) where
  catch (CleanupT action) handler =
    CleanupT $ catch action (unAwareT . handler)

instance MonadMask m => MonadMask (CleanupT m) where
   mask action = CleanupT $ mask (\u -> unAwareT (action $ q u))
    where
      q :: (m a -> m a) -> CleanupT m a -> CleanupT m a
      q u = CleanupT . u . unAwareT

   uninterruptibleMask a =
    CleanupT $ uninterruptibleMask (\u -> unAwareT (a $ q u))
      where
        q :: (m a -> m a) -> CleanupT m a -> CleanupT m a
        q u = CleanupT . u . unAwareT

   generalBracket acquire release use = CleanupT $
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
  => MonadCleanup (CleanupT m) where
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

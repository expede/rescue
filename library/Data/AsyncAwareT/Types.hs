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

module Data.AsyncAwareT.Types
  ( AsyncAwareT (..)
  , AsyncAwareIO
  ) where

import           Control.Monad.Cleanup
import           Data.WorldPeace

-- | Adds 'SomeException' to an Error stack
newtype AsyncAwareT m a = AsyncAwareT { unAwareT :: m a }
 
type AsyncAwareIO a = AsyncAwareT IO a

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

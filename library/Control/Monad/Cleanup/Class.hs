{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | FIXME

module Control.Monad.Cleanup.Class where

import           Data.Functor

import           Control.Monad.IO.Unlift

import Exception hiding (catch, mask, uninterruptibleMask, uninterruptibleMask_)

import           Control.Exception hiding (catch, mask, uninterruptibleMask, uninterruptibleMask_)
import           Control.Monad.Rescue
import           Data.WorldPeace

import           Control.Monad.Raise.Class

import           Control.Monad.Trans.Class

import           Control.Monad.Cont

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import qualified Control.Monad.RWS.Lazy       as Lazy
import qualified Control.Monad.RWS.Strict     as Strict

import qualified Control.Monad.State.Lazy     as Lazy
import qualified Control.Monad.State.Strict   as Strict

import qualified Control.Monad.Writer.Lazy    as Lazy
import qualified Control.Monad.Writer.Strict  as Strict

import           Data.WorldPeace.Subset.Class

import Control.Monad.Catch

newtype UnthrowT m a = UnthrowT { throwable :: m a }

class (Raises SomeException m, MonadRescue m) => MonadCleanup m where
  cleanup
    :: m resource                                        -- ^ acquire some resource
    -- -> (resource -> SomeException        -> m _ignired2) -- ^ sync or async exceptions
    -> (resource -> OpenUnion (Errors m) -> m _ig1)       -- ^ recover -- cleanup, some exception thrown
    -> (resource ->                         m _ig2)        -- ^ Cleanup normally
    -> (resource ->                         m a)         -- ^ inner action to perform with the resource
    -> m a

newtype AsyncAwareT m a = AsyncAwareT { unAware :: m a }
  deriving (Functor, Applicative, Monad)

instance MonadRaise m => MonadRaise (AsyncAwareT m) where
  type Errors (AsyncAwareT m) = SomeException ': Errors m
  -- type Errors (AsyncAwareT m) = SomeAsyncException ': Errors m
  raise = AsyncAwareT . raise

-- newtype AsyncAwareIO a = AsyncAwareIO { unAwareIO :: IO a }

-- instance MonadRaise AsyncAwareIO where
--   type Errors AsyncAwareIO = '[IOException, SomeAsyncException] -- maybe just someexcpetion?
--   raise = AsyncAwareIO . raise

instance MonadRescue (AsyncAwareT IO) where -- FIXME generalize
  attempt (AsyncAwareT action) = AsyncAwareT $
    tryIO action <&> \case
      Right val -> Right val
      Left err  -> Left $ include err

instance MonadThrow m => MonadThrow (AsyncAwareT m) where
  throwM = AsyncAwareT . throwM

instance MonadCatch m => MonadCatch (AsyncAwareT m) where
  catch = undefined -- lift . catch

instance MonadMask m => MonadMask (AsyncAwareT m) where
  mask = undefined . mask
  uninterruptibleMask = undefined . uninterruptableMask

instance MonadCleanup (AsyncAwareT IO) where
  cleanup acquire onErr onOk action =
    mask $ \restore -> do
      resource <- acquire
      attempt (restore $ action resource) >>= \case
        Left errs -> do
          _ <- uninterruptibleMask_ (onErr resource errs `catch` \_ -> return ())
          raise errs

        Right output -> do
          _ <- onOk resource -- output
          return output

-- instance MonadCleanup Maybe where
--   cleanup acquire onErr onOk action = do
--     resource <- acquire
--     attempt (action resource) >>= \case
--       Left err -> do
--         _ <- onErr resource err
--         raise err

--       Right output -> do
--         _ <- onOk resource output
--         return output

-- instance MonadCleanup [] where
--   cleanup acquire onErr onOk action = do
--     resource <- acquire
--     attempt (action resource) >>= \case
--       Left err -> do
--         _ <- onErr resource err
--         raise err

--       Right output -> do
--         _ <- onOk resource output
--         return output

-- instance Contains errs errs => MonadCleanup (Either (OpenUnion errs)) where
--   cleanup acquire onErr onOk action = do
--     resource <- acquire
--     attempt (action resource) >>= \case
--       Left err -> do
--         _ <- onErr resource err
--         raise err

--       Right output -> do
--         _ <- onOk resource output
--         return output

-- unliftedCleanup
--   :: MonadUnliftIO m
--   => m resource                                        -- ^ acquire some resource
--   -> (resource -> OpenUnion (Errors m) -> m _ignored1) -- ^ cleanup, some exception thrown
--   -> (resource -> a                    -> m _ignored2) -- ^ cleanup, no exception thrown. The exception will be rethrown
--   -> (resource ->                         m a)         -- ^ inner action to perform with the resource
--   -> m a

-- unliftedCleanup acquire onErr onOk action =
--   withRunInIO $ \runInIO -> do
--     resource <- acquire
--     attempt (action resource) >>= \case
--       Left err -> do
--         _ <- onErr resource err
--         raise err

--       Right output -> do
--         _ <- onOk resource output
--         return output

-- liftedCleanup
--   :: ( MonadTrans   t
--      , MonadCleanup m
--      , MonadRaise (t m)
--      )
--   => t m resource
--   -> (resource -> OpenUnion (Errors m) -> m _ignored1)
--   -> (resource -> a                    -> m _ignored2)
--   -> (resource ->                         m a)
--   -> t m a
-- liftedCleanup res onErr onOk action = do
--   inner <- res
--   lift $ cleanup (pure inner) onErr onOk action

-- finally :: MonadCleanup m => m a -> m b -> m a

-- cleanRetry :: MonadCleanup m => Nat -> m a -> m a

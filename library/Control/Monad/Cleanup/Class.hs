{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- | FIXME

module Control.Monad.Cleanup.Class where

import           Data.Functor

import           Control.Monad.IO.Unlift

import           Control.Exception
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


class MonadRescue m => MonadCleanup m where -- MonadResource?
  cleanup
    :: m resource                                        -- ^ acquire some resource
    -> (resource -> OpenUnion (Errors m) -> m _ignored1) -- ^ cleanup, some exception thrown
    -> (resource -> a                    -> m _ignored2) -- ^ cleanup, no exception thrown. The exception will be rethrown
    -> (resource ->                         m a)         -- ^ inner action to perform with the resource
    -> m a

instance MonadCleanup IO where
  cleanup acquire onErr onOk action =
    mask $ \restore -> do
      resource <- acquire
      attempt (restore $ action resource) >>= \case
        Left openUnionIOException -> do
          _ <- uninterruptibleMask_ $
            attempt (onErr resource openUnionIOException) <&> \case
              Left  _err -> ()
              Right _val -> ()

          raise openUnionIOException

        Right output -> do
          _ <- onOk resource output
          return output

instance MonadCleanup Maybe where
  cleanup acquire onErr onOk action = do
    resource <- acquire
    attempt (action resource) >>= \case
      Left err -> do
        _ <- onErr resource err
        raise err

      Right output -> do
        _ <- onOk resource output
        return output

instance MonadCleanup [] where
  cleanup acquire onErr onOk action = do
    resource <- acquire
    attempt (action resource) >>= \case
      Left err -> do
        _ <- onErr resource err
        raise err

      Right output -> do
        _ <- onOk resource output
        return output

instance Contains errs errs => MonadCleanup (Either (OpenUnion errs)) where
  cleanup acquire onErr onOk action = do
    resource <- acquire
    attempt (action resource) >>= \case
      Left err -> do
        _ <- onErr resource err
        raise err

      Right output -> do
        _ <- onOk resource output
        return output

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

liftedCleanup
  :: ( MonadTrans   t
     , MonadCleanup m
     , MonadRaise (t m)
     )
  => t m resource
  -> (resource -> OpenUnion (Errors m) -> m _ignored1)
  -> (resource -> a                    -> m _ignored2)
  -> (resource ->                         m a)
  -> t m a
liftedCleanup res onErr onOk action = do
  inner <- res
  lift $ cleanup (pure inner) onErr onOk action

-- finally :: MonadCleanup m => m a -> m b -> m a

-- cleanRetry :: MonadCleanup m => Nat -> m a -> m a

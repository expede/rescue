{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Rescue semantics & helpers
--
-- Essentially a type-directed version of 'Control.Monad.Catch'.
--
-- This is the opposite of 'Control.Monad.Raise', which embeds en error.
-- 'Rescue' takes a potential error out of the surrounding context
-- and either handles or exposes it.

module Control.Monad.Rescue
  ( rescue
  , reattempt
  , onRaise
  , lastly

  -- * TEMP

  , SwapErrorContext (..)
  , handle
  -- , handleOne'

  -- * Reexports

  , module Control.Monad.Raise

  , module Control.Monad.Rescue.Class
  , module Control.Monad.Rescue.Constraint
  ) where

import           Data.Result.Types
import           Data.WorldPeace

import           Control.Monad.Raise

import           Control.Monad.Rescue.Class
import           Control.Monad.Rescue.Constraint

import           Numeric.Natural

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
--
-- >>> import Control.Monad.Trans.Rescue
-- >>> import Data.Proxy
-- >>> import Data.WorldPeace as OpenUnion
--
-- >>> data FooErr  = FooErr  deriving Show
-- >>> data BarErr  = BarErr  deriving Show
-- >>> data QuuxErr = QuuxErr deriving Show

-- | Handle all exceptions
--
-- >>> type MyErrs = '[FooErr, BarErr]
-- >>> myErrs = Proxy @MyErrs
--
-- >>> :{
-- goesBoom :: Int -> Rescue MyErrs String
-- goesBoom x =
--   if x > 50
--     then return (show x)
--     else raise FooErr
-- :}
--
-- >>> handler = catchesOpenUnion (\foo -> "Foo: " <> show foo, \bar -> "Bar:" <> show bar)
-- >>> rescue (goesBoom 42) (pure . handler)
-- RescueT (Identity (Right "Foo: FooErr"))
rescue
  :: ( MonadRescue m
     , RaisesOnly  m errs
     )
  => m a
  -> (OpenUnion errs -> m a)
  -> m a
rescue action handler = either handler pure =<< attempt action

onRaise
  :: ( MonadRescue m
     , RaisesOnly  m errs
     )
  => (OpenUnion errs -> m ())
  -> m a
  -> m (Result errs a)
onRaise errHandler action =
  attempt action >>= \case
    Left err -> do
      errHandler err
      return $ Err err

    Right val ->
      return $ Ok val

-- | 'retry' without asynchoronous exception cleanup.
--   Useful when not dealing with external resources that may
--   be dangerous to close suddenly.
reattempt :: MonadRescue m => Natural -> m a -> m a
reattempt 0     action = action
reattempt times action =
  attempt action >>= \case
    Left  _   -> reattempt (times - 1) action
    Right val -> return val

-- | Run an additional step, and throw away the result.
--   Return the result of the action passed.
lastly :: (Contains (Errors m) (Errors n), MonadRaise n, MonadRescueTo m n) => m a -> n b -> n a
lastly action finalizer = do
  errOrOk <- attempt action
  _       <- finalizer
  ensure errOrOk

handle
  :: ( MonadRaise n
     , MonadRescueTo m n
     , ElemRemove err (Errors m)
     , Contains (Remove err (Errors m)) (Errors n)
     )
  => m a
  -> (m (Either (OpenUnion (Errors m)) a) -> n (Either (OpenUnion (Errors m)) a))
  -> (err -> n a)
  -> n a
handle action handler = do
  attempt action >>= \case
    Right val ->
      return val

    Left err ->
      openUnionHandle raise handler err

handle
  :: ( MonadRaise n
     , MonadRescueTo m n
     , ElemRemove err (Errors m)
     , Contains (Remove err (Errors m)) (Errors n)
     )
  => m a
  -> (m (Either (OpenUnion (Errors m)) a) -> n (Either (OpenUnion (Errors m)) a))
  -> (err -> n a)
  -> n a
hanldeWith handler action = action handler

-- handleOne
--   :: ( LocalExceptionContext m n
--      , MonadRaise  n
--      , MonadRescue m
--      , ElemRemove err (Errors m)
--      , Contains (Remove err (Errors m)) (Errors n)
--      )
--   => m a
--   -> (err -> n a)
--   -> n a
-- handleOne action handler = handleOne' action localErrorContext handler

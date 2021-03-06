{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Rescue semantics & helpers
--
-- Essentially a type-directed version of 'Control.Monad.Catch'.
--
-- This is the opposite of 'Control.Monad.Raise', which embeds en error.
-- 'Rescue' takes a potential error out of the surrounding context
-- and either handles or exposes it.

module Control.Monad.Rescue
  ( rescue
  , handle

  -- * Guaranteed runs

  , reattempt
  , onRaise
  , lastly

  -- * Error access

  , mapError
  , replaceError
  , asNotFound

  -- * Reexports

  , module Control.Monad.Raise

  , module Control.Monad.Rescue.Class
  , module Control.Monad.Rescue.Constraint
  ) where

import           Data.Exception.Types
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
-- >>> rescue (goesBoom 42) (pure . handler) :: Rescue MyErrs String
-- RescueT (Identity (Right "Foo: FooErr"))
rescue
  :: MonadRescueFrom n m
  => n a
  -> (ErrorCase n -> m a)
  -> m a
rescue action handler = either handler pure =<< attempt action

handle
  :: ( MonadRaise        m
     , MonadRescueFrom n m
     , Handles     err n m
     )
  => n a
  -> (err -> m a)
  -> m a
handle action handler =
  attempt action >>= \case
    Left err    -> openUnionHandle raise handler err
    Right value -> return value

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
lastly
  :: ( Errors m `Contains` Errors m
     , MonadRaise m
     , MonadRescueFrom m m
     )
  => m a
  -> m b
  -> m a
lastly action finalizer = do
  errOrOk <- attempt action
  _       <- finalizer
  ensure errOrOk

-- AKA reinterpret
mapError ::
  ( n `MonadRescueFrom` m
  , MonadRaise m
  , Errors m `Contains` Errors m
  )
  => (ErrorCase n -> ErrorCase m)
  -> n a
  -> m a
mapError mapper action =
  attempt action >>= \case
    Left  errCaseN -> raise $ mapper errCaseN
    Right value    -> return value

replaceError ::
  ( n `MonadRescueFrom` m
  , MonadRaise m
  , m `Raises` err
  )
  => err
  -> n a
  -> m a
replaceError err action =
  attempt action >>= \case
    Left  _     -> raise err
    Right value -> return value

asNotFound :: forall n m a .
  ( n `MonadRescueFrom` m
  , m `Raises` NotFound a
  , MonadRaise m
  )
  => n a
  -> m a
asNotFound action = replaceError (NotFound @a) action

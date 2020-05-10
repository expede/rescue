{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Rescue semantics & helpers
--
-- Essentially a type-directed version of 'Control.Monad.Catch'.
--
-- This is the opposite of 'Control.Monad.Raise', which embeds en error.
-- 'Rescue' takes a potential error out of the surrounding context
-- and either handles or exposes it.

module Control.Monad.Rescue
  ( rescue
  , cleanup
  , finally
  , ensureM

  -- * Reexports

  , module Control.Monad.Raise
  , module Control.Monad.Rescue.Class
  ) where

import           Data.WorldPeace

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class

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

-- | FIXME add one-liner
--
-- >>> type MyErrs = '[FooErr, BarErr]
-- >>> myErrs = Proxy @MyErrs
--
-- >>> :{
-- goesBoom :: Int -> Rescue MyErrs String
-- goesBoom x =
--   if x > 50
--     then return (show x)
--     else raiseAs @MyErrs FooErr
-- :}
--
-- >>> handler = catchesOpenUnion (\foo -> "Foo: " <> show foo, \bar -> "Bar:" <> show bar)
-- >>> rescue myErrs (goesBoom 42) (pure . handler)
-- RescueT (Identity (Right "Foo: FooErr"))
rescue :: MonadRescue errs m => m a -> (OpenUnion errs -> m a) -> m a
rescue action handler = either handler pure =<< try action

-- | A version of @ensure@ that takes monadic actions
--
-- ==== __Examples__
--
-- >>> :{
--   mayFailM :: Monad m => Int -> m (Either (OpenUnion MyErrs) Int)
--   mayFailM n =
--     return $ if n > 50
--       then Left (openUnionLift FooErr)
--       else Right n
-- :}
--
-- >>> type BigErrs = '[FooErr, BarErr, QuuxErr]
--
-- >>> :{
--   foo :: MonadRaise BigErrs m => m Int
--   foo = do
--     first  <- ensureM @BigErrs $ mayFailM 100
--     second <- ensureM @BigErrs $ mayFailM first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensureM :: forall outer inner m a .
  ( Contains inner outer
  , MonadRaise (OpenUnion outer) m
  )
  => m (Either (OpenUnion inner) a)
  -> m a
ensureM action = ensure @outer =<< action

finally :: forall errs m a b .
  ( MonadRaise (OpenUnion errs) m -- FIXME probably can be cut
  , MonadRescue errs m
  )
  => m a
  -> m b
  -> m a
finally action finalizer =
  try @errs action >>= \case
    Left  err -> finalizer >> raise err
    Right val -> finalizer >> pure  val

cleanup ::
  ( MonadRaise (OpenUnion errs) m -- FIXME probably can be cut
  , MonadRescue errs m
  )
  => m resource
  -> (resource -> OpenUnion errs -> m _ignored1)
  -> (resource ->   a            -> m _ignored2)
  -> (resource -> m a)
  -> m a
cleanup acquire onErr onOk action = do
  resource <- acquire
  try (action resource) >>= \case
    Left  err    -> onErr resource err    >> raise err
    Right output -> onOk  resource output >> pure output

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
  , retry

  -- * Reexports

  , module Control.Monad.Raise
  , module Control.Monad.Rescue.Class
  ) where

import           Data.WorldPeace

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class

import           Numeric.Natural

-- FIXME raiseIn?

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

-- onException :: MonadCatch m => m a -> m b -> m a
-- onRaise     :: MonadRescue m => m a -> m b -> m a

retry :: MonadRescue m => Natural -> m a -> m a
retry 0     action = action
retry times action =
  attempt action >>= \case
    Left  _   -> retry (times - 1) action
    Right val -> return val

-- finallySync
--   :: ( Contains (Errors m) (Errors m) -- FIXME WUUUUUUUT why is this needed?
--      , MonadRescue m
--      )
--   => m a
--   -> m b
--   -> m a
-- finallySync action finalizer = do
--   errOrOk <- attempt action
--   _       <- finalizer
--   ensure errOrOk

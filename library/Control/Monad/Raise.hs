{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Monadic raise semantics & helpers

module Control.Monad.Raise
  ( ensure
  -- * Reexports
  , module Control.Monad.Raise.Class
  ) where

import           Data.WorldPeace

import           Control.Monad.Raise.Class

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
-- >>>
-- >>> import Data.Proxy
-- >>> import Data.WorldPeace
-- >>>
-- >>> data FooErr  = FooErr
-- >>> data BarErr  = BarErr
-- >>> data QuuxErr = QuuxErr

-- | Lift a pure error (@Either@) into a @MonadRaise@ context
-- i.e. Turn @Left@s into @raise@s.
--
-- ==== __Examples__
--
-- >>> :{
--   mayFail :: Int -> Either (OpenUnion MyErrs) Int
--   mayFail n =
--     if n > 50
--       then Left (openUnionLift FooErr)
--       else Right n
-- :}
--
-- >>> :{
--   foo :: MonadRaise MyErrs m => m Int
--   foo = do
--     first  <- ensure @MyErrs $ mayFail 100
--     second <- ensure @MyErrs $ mayFail first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensure :: forall outer inner m a .
  ( Contains inner outer
  , MonadRaise (OpenUnion outer) m
  )
  => Either (OpenUnion inner) a
  -> m a
ensure = \case
  Left  err -> raise @(OpenUnion outer) (relaxOpenUnion err)
  Right val -> pure val

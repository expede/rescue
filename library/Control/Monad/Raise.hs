{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE LambdaCase   #-}

-- | Monadic raise semantics & helpers

module Control.Monad.Raise
  (
  -- * Reexports
   
    module Control.Monad.Raise.Class

  -- * 'raise' Helpers
 
  -- , raiseAs
  -- , raiseTo

  -- * 'ensure' Helpers

  -- ** On Bare Errors

  -- , ensureAs
  -- , ensureAsM

  -- ** On Error Collections

  , ensure
  -- , ensureM
  ) where

import           Control.Monad.Raise.Class

import           Data.WorldPeace
 
import           Rescue.Internal.Data.WorldPeace

import Control.Monad.Foo

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
-- >>>
-- >>> type MyErrs = '[FooErr, BarErr]

-- -- | Raise an open sum error
-- --
-- -- A specialized version of @raise@,
-- -- which infers taht the error context is an exact match
-- --
-- -- ==== __Examples__
-- --
-- -- >>> let fooErr = openUnionLift FooErr :: OpenUnion MyErrs
-- -- >>>
-- -- >>> :{
-- --  goesBoom :: MonadRaise MyErrs m => Int -> m Int
-- --  goesBoom x =
-- --    if x > 50
-- --      then return x
-- --      else raise' fooErr
-- -- :}
-- --
-- -- >>> goesBoom 42 :: Maybe Int
-- -- Nothing
-- raise' :: forall errs m a . MonadRaise errs m => OpenUnion errs -> m a
-- raise' = raise (Proxy @errs)

-- -- | Raise a single error into a particular context
-- --
-- -- ==== __Examples__
-- --
-- -- >>> raiseAs @MyErrs FooErr :: Maybe Int
-- -- Nothing
-- raiseAs :: forall errs err m a . (IsMember err errs, MonadRaise errs m) => err -> m a
-- raiseAs = raise . liftAs @errs

-- -- | Raise an existing error union to a wider union
-- --
-- -- ==== __Examples__
-- --
-- -- >>> let smallErr = openUnionLift FooErr :: OpenUnion MyErrs
-- -- >>> type BigErrs = '[FooErr, BarErr, QuuxErr]
-- -- >>> raiseTo @BigErrs smallErr :: Maybe Int
-- -- Nothing
-- raiseTo :: forall outer inner m a .
--   ( Contains inner outer
--   , MonadRaise outer m
--   )
--   => OpenUnion inner
--   -> m a
-- raiseTo = raise @outer . relaxTo @outer

-- -- | Lift a pure result to a @MonadRaise@ context
-- --
-- -- ==== __Examples__
-- --
-- -- >>> :{
-- --   mayFail :: Int -> Either FooErr Int
-- --   mayFail n =
-- --     if n > 50
-- --       then Left FooErr
-- --       else Right n
-- -- :}
-- --
-- -- >>> :{
-- --   foo :: MonadRaise MyErrs m => m Int
-- --   foo = do
-- --     first  <- ensureAs @MyErrs $ mayFail 100
-- --     second <- ensureAs @MyErrs $ mayFail first
-- --     return (second * 10)
-- -- :}
-- --
-- -- >>> foo :: Maybe Int
-- -- Nothing
-- ensureAs :: forall errs err m a .
--   ( ToOpenUnion err errs
--   , MonadRaise errs m
--   )
--   => Either err a
--   -> m a
-- ensureAs = either (raise @errs) pure

-- -- | Like @ensure1@, but takes a monadic argument
-- --
-- -- ==== __Examples__
-- --
-- -- >>> :{
-- --   mayFailM :: Monad m => Int -> m (Either FooErr Int)
-- --   mayFailM n =
-- --     return $ if n > 50
-- --       then Left FooErr
-- --       else Right n
-- -- :}
-- --
-- -- >>> :{
-- --   foo :: MonadRaise MyErrs m => m Int
-- --   foo = do
-- --     first  <- ensureAsM @MyErrs $ mayFailM 100
-- --     second <- ensureAsM @MyErrs $ mayFailM first
-- --     return (second * 10)
-- -- :}
-- --
-- -- >>> foo :: Maybe Int
-- -- Nothing
-- ensureAsM :: forall errs err m a .
--   ( IsMember err errs
--   , MonadRaise errs m
--   )
--   => m (Either err a)
--   -> m a
-- ensureAsM action = ensureAs @errs =<< action

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
ensure :: forall outer inner m a errs .
  ( ToOpenUnion inner outer
  , MonadRaise (OpenUnion outer) m
  , errs ~ OpenUnion outer
  )
  => Either inner a
  -> m a
ensure = \case
  Left  err -> raise @errs (consistent err)
  Right val -> pure val

-- -- | A version of @ensure@ that takes monadic actions
-- --
-- -- ==== __Examples__
-- --
-- -- >>> :{
-- --   mayFailM :: Monad m => Int -> m (Either (OpenUnion MyErrs) Int)
-- --   mayFailM n =
-- --     return $ if n > 50
-- --       then Left (openUnionLift FooErr)
-- --       else Right n
-- -- :}
-- --
-- -- >>> type BigErrs = '[FooErr, BarErr, QuuxErr]
-- --
-- -- >>> :{
-- --   foo :: MonadRaise BigErrs m => m Int
-- --   foo = do
-- --     first  <- ensureM @BigErrs $ mayFailM 100
-- --     second <- ensureM @BigErrs $ mayFailM first
-- --     return (second * 10)
-- -- :}
-- --
-- -- >>> foo :: Maybe Int
-- -- Nothing
-- ensureM :: forall outer inner m a errs .
--   ( ToOpenUnion inner outer
--   , MonadRaise (OpenUnion outer) m
--   , errs ~ OpenUnion outer
--   )
--   => m (Either inner a)
--   -> m a
-- ensureM action = either (raise @errs . consistent) pure =<< action

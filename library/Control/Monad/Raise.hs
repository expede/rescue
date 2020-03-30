{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Monadic raise semantics & helpers

module Control.Monad.Raise
  (
  -- * Reexports
   
    module Control.Monad.Raise.Class

  -- * 'raise' Helpers
 
  , raise'
  , raiseAs
  , raiseTo

  -- * 'ensure' Helpers

  -- ** On Bare Errors

  , ensureAs
  , ensureAsM

  -- ** On Error Collections

  , ensure
  , ensureM
  ) where

import           Control.Monad.Raise.Class

import           Data.Proxy
import           Data.WorldPeace
 
import           Rescue.Internal.Data.WorldPeace

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

-- | Raise an open sum error
--
-- A specialized version of @raise@,
-- which infers taht the error context is an exact match
--
-- ==== __Examples__
--
-- >>> let fooErr = openUnionLift FooErr :: OpenUnion MyErrs
-- >>>
-- >>> :{
--  goesBoom :: MonadRaise MyErrs m => Int -> m Int
--  goesBoom x =
--    if x > 50
--      then return x
--      else raise' fooErr
-- :}
--
-- >>> goesBoom 42 :: Maybe Int
-- Nothing
raise' :: forall errs m a . MonadRaise errs m => OpenUnion errs -> m a
raise' = raise (Proxy @errs)

-- | Raise a single error into a particular context
--
-- ==== __Examples__
--
-- >>> raiseAs (Proxy @MyErrs) FooErr :: Maybe Int
-- Nothing
raiseAs :: (IsMember err errs, MonadRaise errs m) => Proxy errs -> err -> m a
raiseAs proxy = raise' . liftAs proxy

-- | Raise an existing error union to a wider union
--
-- ==== __Examples__
--
-- >>> let smallErr = openUnionLift FooErr :: OpenUnion MyErrs
-- >>> type BigErrs = '[FooErr, BarErr, QuuxErr]
-- >>> bigErrs = Proxy @BigErrs
-- >>> raiseTo bigErrs smallErr :: Maybe Int
-- Nothing
raiseTo :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> OpenUnion inner
  -> m a
raiseTo proxy = raise' . relaxTo proxy

-- | Lift a pure result to a @MonadRaise@ context
--
-- ==== __Examples__
--
-- >>> let errs = Proxy @MyErrs
--
-- >>> :{
--   mayFail :: Int -> Either FooErr Int
--   mayFail n =
--     if n > 50
--       then Left FooErr
--       else Right n
-- :}
--
-- >>> :{
--   foo :: MonadRaise MyErrs m => m Int
--   foo = do
--     first  <- ensureAs errs $ mayFail 100
--     second <- ensureAs errs $ mayFail first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensureAs :: forall m a err errs.
  (IsMember err errs, MonadRaise errs m) => Proxy errs -> Either err a -> m a
ensureAs pxy = either (raiseAs pxy) pure

-- | Like @ensure1@, but takes a monadic argument
--
-- ==== __Examples__
--
-- >>> :{
--   mayFailM :: Monad m => Int -> m (Either FooErr Int)
--   mayFailM n =
--     return $ if n > 50
--       then Left FooErr
--       else Right n
-- :}
--
-- >>> let errs = Proxy @MyErrs
--
-- >>> :{
--   foo :: MonadRaise MyErrs m => m Int
--   foo = do
--     first  <- ensureAsM errs $ mayFailM 100
--     second <- ensureAsM errs $ mayFailM first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensureAsM :: forall err errs m a .
  (IsMember err errs, MonadRaise errs m) => Proxy errs -> m (Either err a) -> m a
ensureAsM pxy action = ensureAs pxy =<< action

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
-- >>> let errs = Proxy @MyErrs
--
-- >>> :{
--   foo :: MonadRaise MyErrs m => m Int
--   foo = do
--     first  <- ensure errs $ mayFail 100
--     second <- ensure errs $ mayFail first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensure :: forall m a inner outer .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> Either (OpenUnion inner) a
  -> m a
ensure pxy = either (raiseTo pxy) pure

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
-- >>> let errs = Proxy @BigErrs
--
-- >>> :{
--   foo :: MonadRaise BigErrs m => m Int
--   foo = do
--     first  <- ensureM errs $ mayFailM 100
--     second <- ensureM errs $ mayFailM first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensureM :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> m (Either (OpenUnion inner) a)
  -> m a
ensureM pxy action = either (raiseTo pxy) pure =<< action

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Monadic raise semantics

module Control.Monad.Raise
  ( module Control.Monad.Raise.Class

  , raise
  , raiseAs
  , raiseTo

  , ensure
  , ensureM

  , ensure'
  , ensureM'

  , ensure1
  , ensureM1
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
-- >>> type MyErrs  = '[FooErr, BarErr]

-- | Raise an open sum error
--
-- A specialized version of @raise'@,
-- which infers taht the error context is an exact match
--
-- Examples:
--
-- >>> let fooErr = openUnionLift FooErr :: OpenUnion MyErrs
--
-- >>> :{
--  goesBoom :: MonadRaise MyErrs m => Int -> m Int
--  goesBoom x =
--    if x > 50
--      then return x
--      else raise fooErr
-- :}
--
-- >>> goesBoom 42 :: [Int]
-- []
raise :: forall errs m a . MonadRaise errs m => OpenUnion errs -> m a
raise = raise' (Proxy @errs)

-- | Raise a single error into a particular context
--
-- Examples:
--
-- >>> let boom = raiseAs (Proxy @MyErrs) FooErr
-- >>> boom :: [Int]
-- []
raiseAs :: (IsMember err errs, MonadRaise errs m) => Proxy errs -> err -> m a
raiseAs proxy = raise . liftAs proxy

-- | Raise an existing error union to a wider union
--
-- Examples:
--
-- >>> let smallErr = openUnionLift FooErr :: OpenUnion MyErrs
-- >>> type BigErrs = '[FooErr, BarErr, QuuxErr]
-- >>>
-- >>> let boom = raiseTo (Proxy @BigErrs) smallErr
-- >>> boom :: [Int]
-- []
raiseTo :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> OpenUnion inner
  -> m a
raiseTo proxy = raise . relaxTo proxy

-- | Lift a pure result to a @MonadRaise@ context
--
-- Examples:
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
--     first  <- ensure1 errs $ mayFail 100
--     second <- ensure1 errs $ mayFail first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensure1 :: forall m a err errs.
  (IsMember err errs, MonadRaise errs m) => Proxy errs -> Either err a -> m a
ensure1 pxy = either (raiseAs pxy) pure

-- | Like @ensure1@, but takes a monadic argument
--
-- Examples:
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
--     first  <- ensureM1 errs $ mayFailM 100
--     second <- ensureM1 errs $ mayFailM first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensureM1 :: forall err errs m a .
  (IsMember err errs, MonadRaise errs m) => Proxy errs -> m (Either err a) -> m a
ensureM1 pxy action = ensure1 pxy =<< action


-- | ------------------------------------------------------
--
-- Examples:
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
--     first  <- ensure' errs $ mayFail 100
--     second <- ensure' errs $ mayFail first
--     return (second * 10)
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensure' :: forall m a inner outer .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> Either (OpenUnion inner) a
  -> m a
ensure' pxy = either (raiseTo pxy) pure

ensure :: forall m a inner outer .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Either (OpenUnion inner) a
  -> m a
ensure = ensure' (Proxy @outer)

ensureM' :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> m (Either (OpenUnion inner) a)
  -> m a
ensureM' pxy action = either (raiseTo pxy) pure =<< action

ensureM :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => m (Either (OpenUnion inner) a)
  -> m a
ensureM = ensureM' (Proxy @outer)

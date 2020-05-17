{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Monadic raise semantics & helpers

module Control.Monad.Raise
  ( ensure
  , ensureM
  -- * Class Reexports
  , module Control.Monad.Raise.Class
  , module Control.Monad.Raise.Constraint
  -- * Data Reexports
  , module Data.WorldPeace.Subset.Class
  ) where

import           Control.Monad.Raise.Class
import           Control.Monad.Raise.Constraint

import           Data.WorldPeace.Subset.Class

-- FIXME add that monolocalbinds is needed to the docs for the doctest

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XMonoLocalBinds
-- >>> :set -XTypeApplications
-- >>>
-- >>> import Data.Proxy
-- >>> import Data.Result
-- >>> import Data.WorldPeace
-- >>>
-- >>> data FooErr  = FooErr  deriving Show
-- >>> data BarErr  = BarErr  deriving Show
-- >>> data QuuxErr = QuuxErr deriving Show

-- | Lift a pure error (@Either@) into a @MonadRaise@ context
-- i.e. Turn @Left@s into @raise@s.
--
-- ==== __Examples__
--
-- FIXME more examples for more cases of the automated behaviour!!
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
--   goesBoom :: (MonadRaise m, Raises FooErr m) => m Int
--   goesBoom = do
--     first  <- ensure $ mayFail 100
--     second <- ensure $ mayFail 42
--     return $ second * 10
-- :}
--
-- >>> goesBoom :: Result '[FooErr, BarErr] Int
-- Left (Identity FooErr)
ensure :: (MonadRaise m, Raises inner m) => Either inner a -> m a
ensure (Right val) = pure val
ensure (Left err)  = raise err

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
ensureM
  :: ( MonadRaise   m
     , Raises inner m
     )
  => m (Either inner a)
  -> m a
ensureM action = ensure =<< action

-- FIXME TODO Make a "Forget" function? i.e. ensure FooErr -> ensure ()

-- FIXME a PluckError/RemoveError/HandlesError constraint for Rescue?
-- Handles err m n = (Raises err m, MonadRaise m, MonadRescue (Remove err (Errs m)) n)

-- FIXME the constraint errros can get hairy. Add some custom errors :wink:

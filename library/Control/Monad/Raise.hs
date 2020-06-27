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
  , onException

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
-- >>> :set -XTypeFamilies
-- >>> :set -XTypeOperators
-- >>>
-- >>> import Control.Monad.Trans.Rescue
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
-- >>> :{
--   mayFail :: Int -> Either FooErr Int
--   mayFail n =
--     if n > 50
--       then Left FooErr
--       else Right n
-- :}
--
-- >>> :{
--   goesBoom :: (MonadRaise m, m `Raises` FooErr) => m Int
--   goesBoom = do
--     first  <- ensure $ mayFail 100
--     second <- ensure $ mayFail 42
--     return $ second * 10
-- :}
--
-- >>> goesBoom :: Result '[FooErr, BarErr] Int
-- Left (Identity FooErr)
ensure :: (MonadRaise m, Raises m inner) => Either inner a -> m a
ensure (Right val) = pure val
ensure (Left err)  = raise err

-- | A version of @ensure@ that takes monadic actions
--
-- ==== __Examples__
--
-- >>> :{
--   mayFailM :: Monad m => Int -> m (Either (OpenUnion '[FooErr, BarErr]) Int)
--   mayFailM n =
--     return $ if n > 50
--       then Left (openUnionLift FooErr)
--       else Right n
-- :}
--
-- >>> :{
--   foo :: (MonadRaise m, RaisesOnly m '[FooErr, BarErr]) => m Int
--   foo = do
--     first  <- ensureM $ mayFailM 100
--     second <- ensureM $ mayFailM first
--     return (second * 10)
-- :}
--
-- >>> runRescue (foo :: Rescue '[FooErr, BarErr] Int)
-- Left (Identity FooErr)
ensureM
  :: ( MonadRaise m
     , Raises     m inner
     )
  => m (Either inner a)
  -> m a
ensureM action = ensure =<< action

onException
  :: Monad m
  => (err -> m ())
  -> m (Either err a)
  -> m (Either err a)
onException errHandler action =
  action >>= \case
    Left err -> do
      errHandler err
      return $ Left err

    Right val ->
      return $ Right val

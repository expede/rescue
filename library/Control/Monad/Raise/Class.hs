{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME

module Control.Monad.Raise.Class (MonadRaise (..)) where

import           Control.Monad.Cont
import           Data.WorldPeace

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
--
-- >>> import Data.Proxy
-- >>> import Data.WorldPeace

-- | Raise semantics, like a type-directed @MonadThrow@
class Monad m => MonadRaise err m where
  -- | Raise an error
  --
  -- The @Proxy@ gives a type hint to the type checker.
  -- If you have a case where it can be inferred, see 'Control.Monad.Raise.raise''.
  --
  -- ==== __Examples__
  --
  -- >>> data FooErr  = FooErr
  -- >>> data BarErr  = BarErr
  -- >>> data QuuxErr = QuuxErr
  -- >>>
  -- >>> type MyErrs  = '[FooErr, BarErr]
  -- >>> myErrs = Proxy @MyErrs
  -- >>>
  -- >>> let fooErr = openUnionLift FooErr :: OpenUnion MyErrs
  -- >>>
  -- >>> :{
  --  goesBoom x =
  --    if x > 50
  --      then return x
  --      else raise @MyErrs fooErr
  -- :}
  --
  -- >>> goesBoom 42 :: Maybe Int
  -- Nothing
  raise :: err -> m a

instance MonadRaise errs [] where
  raise _ = []

instance MonadRaise errs Maybe where
  raise _ = Nothing

instance MonadRaise (OpenUnion errs) (Either (OpenUnion errs)) where
  raise err = Left err

instance IsMember err errs => MonadRaise err (Either (OpenUnion errs)) where
  raise err = Left $ openUnionLift err

instance Contains inner outer => MonadRaise (OpenUnion inner) (Either (OpenUnion outer)) where
  raise err = Left $ relaxOpenUnion err

instance (MonadTrans t, Monad (t m), MonadRaise errs m) => MonadRaise errs (t m) where
  raise = lift . raise

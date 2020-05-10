{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- |

module Control.Monad.Raise.Class (MonadRaise (..)) where

import           Data.Proxy
import           Data.WorldPeace

import           Control.Monad.Catch.Pure
import           Control.Monad.Cont

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import qualified Control.Monad.RWS.Lazy   as Lazy
import qualified Control.Monad.RWS.Strict as Strict

import qualified Control.Monad.State.Lazy   as Lazy
import qualified Control.Monad.State.Strict as Strict

import qualified Control.Monad.Writer.Lazy   as Lazy
import qualified Control.Monad.Writer.Strict as Strict





import           Control.Monad.Foo

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
 
instance ToOpenUnion errs err => MonadRaise err (Either (OpenUnion errs)) where
  raise err = Left $ consistent err

instance (MonadTrans t, Monad (t m), MonadRaise errs m) => MonadRaise errs (t m) where
  raise = lift . raise

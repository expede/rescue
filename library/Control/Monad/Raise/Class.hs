{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME

module Control.Monad.Raise.Class (MonadRaise (..)) where --, ToOpenUnion (..)) where

import           Control.Monad.Cont
import           Control.Monad.Catch.Pure

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

instance MonadRaise err (Either err) where
  raise = Left

instance IsMember err errs => MonadRaise err (Either (OpenUnion errs)) where
  raise = Left . openUnionLift

instance Contains inner outer
  => MonadRaise (OpenUnion inner) (Either (OpenUnion outer)) where
    raise = Left . relaxOpenUnion
 
instance Monad m => MonadRaise err (ExceptT err m) where
  raise = ExceptT . pure . Left

instance
  ( Monad m
  , IsMember err errs
  )
  => MonadRaise err (ExceptT (OpenUnion errs) m) where
    raise = ExceptT . pure . Left . openUnionLift

instance
  ( Monad m
  , Contains inner outer
  )
  => MonadRaise (OpenUnion inner) (ExceptT (OpenUnion outer) m) where
    raise = ExceptT . pure . Left . relaxOpenUnion

instance MonadRaise errs m => MonadRaise errs (MaybeT m) where
  raise = lift . raise

instance MonadRaise errs m => MonadRaise errs (IdentityT m) where
  raise = lift . raise

instance MonadRaise errs m => MonadRaise errs (ReaderT cfg m) where
  raise = lift . raise

instance MonadRaise errs m => MonadRaise errs (CatchT m) where
  raise = lift . raise

instance MonadRaise errs m => MonadRaise errs (ContT r m) where
  raise = lift . raise

instance MonadRaise errs m => MonadRaise errs (Lazy.StateT s m) where
  raise = lift . raise

instance MonadRaise errs m => MonadRaise errs (Strict.StateT s m) where
  raise = lift . raise

instance
  ( MonadRaise errs m
  , Monoid w
  )
  => MonadRaise errs (Lazy.WriterT w m) where
    raise = lift . raise

instance
  ( MonadRaise errs m
  , Monoid w
  )
  => MonadRaise errs (Strict.WriterT w m) where
    raise = lift . raise

instance
  ( MonadRaise errs m
  , Monoid w
  )
  => MonadRaise errs (Lazy.RWST r w s m) where
    raise = lift . raise

instance
  ( MonadRaise errs m
  , Monoid w
  )
  => MonadRaise errs (Strict.RWST r w s m) where
    raise = lift . raise

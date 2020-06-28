{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | FIXME docs

module Control.Monad.Raise.Class (MonadRaise (..)) where

import           Control.Exception

import           Control.Monad.Catch.Pure
import           Control.Monad.Cont

import           Control.Monad.ST

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import qualified Control.Monad.RWS.Lazy       as Lazy
import qualified Control.Monad.RWS.Strict     as Strict

import qualified Control.Monad.State.Lazy     as Lazy
import qualified Control.Monad.State.Strict   as Strict

import qualified Control.Monad.Writer.Lazy    as Lazy
import qualified Control.Monad.Writer.Strict  as Strict

import           Data.Kind
import           Data.WorldPeace
import           Data.WorldPeace.Subset.Class

import           GHC.Base
import           GHC.Conc
import           GHC.IO

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
--
-- >>> import Data.WorldPeace

-- | Raise semantics, like a type-directed @MonadThrow@.
--   Not unlike @MonadError@ with an in-built open variant.
class Monad m => MonadRaise m where
  type Errors m :: [Type]

  -- | Raise an error
  --
  -- The @Proxy@ gives a type hint to the type checker.
  -- If you have a case where it can be inferred, see 'Control.Monad.Raise.raise''.
  --
  -- ==== __Examples__
  --
  -- >>> data FooErr  = FooErr  deriving Show
  -- >>> data BarErr  = BarErr  deriving Show
  -- >>> data QuuxErr = QuuxErr deriving Show
  -- >>>
  -- >>> type MyErrs  = '[FooErr, BarErr]
  -- >>>
  -- >>> :{
  --  goesBoom :: Int -> Either (OpenUnion MyErrs) Int
  --  goesBoom x =
  --    if x > 50
  --      then return x
  --      else raise FooErr
  -- :}
  --
  -- >>> goesBoom 42
  -- Left (Identity FooErr)
  --
  -- >>> :{
  --  maybeBoom :: Int -> Maybe Int
  --  maybeBoom x =
  --    if x > 50
  --      then return x
  --      else raise ()
  -- :}
  --
  -- >>> maybeBoom 42
  -- Nothing
  raise :: Subset err (OpenUnion (Errors m)) => err -> m a

instance MonadRaise [] where
  type Errors [] = '[()]
  raise _ = []

instance MonadRaise Maybe where
  type Errors Maybe = '[()]
  raise _ = Nothing

instance MonadRaise (Either (OpenUnion errs)) where
  type Errors (Either (OpenUnion errs)) = errs
  raise = Left . include

instance MonadRaise IO where
  type Errors IO = '[IOException]
  raise = IO . raiseIO#

instance MonadRaise (ST s) where
  type Errors (ST s) = '[IOException]
  raise = GHC.IO.unsafeIOToST . raise

instance MonadRaise STM where
  type Errors STM = '[IOException]
  raise = STM . raiseIO#

-- NOTE behaves differently from MonadThrow!
-- with `lift . raise`, you must have `errs ~ Errors m`
-- This implementation ONLY gives you access to the inner Either
-- FIXME make sure this is sane
-- instance MonadRaise m => MonadRaise (ExceptT (OpenUnion errs) m) where
--   type Errors (ExceptT (OpenUnion errs) m) = errs
--   raise = ExceptT . pure . raise

-- NOTE the version below doesn't make sense, because `IO (Either (OpenUnion errs) a`
-- This version ensures that the inner errs contains the type of the outer errs
-- The main user case is to merge IOException with pure errs
-- But may cause issues with async vs sync errors
instance (MonadRaise m, Contains (Errors m) errs)
  => MonadRaise (ExceptT (OpenUnion errs) m) where
    type Errors (ExceptT (OpenUnion errs) m) = errs
    raise = ExceptT . pure . raise

instance MonadRaise m => MonadRaise (IdentityT m) where
  type Errors (IdentityT m) = Errors m
  raise = lift . raise

instance MonadRaise m => MonadRaise (MaybeT m) where
  type Errors (MaybeT m) = Errors m
  raise = lift . raise

instance MonadRaise m => MonadRaise (ReaderT cfg m) where
  type Errors (ReaderT cfg m) = Errors m
  raise = lift . raise

instance MonadRaise m => MonadRaise (CatchT m) where
  type Errors (CatchT m) = Errors m
  raise = lift . raise

instance MonadRaise m => MonadRaise (ContT r m) where
  type Errors (ContT r m) = Errors m
  raise = lift . raise

instance MonadRaise m => MonadRaise (Lazy.StateT s m) where
  type Errors (Lazy.StateT s m) = Errors m
  raise = lift . raise

instance MonadRaise m => MonadRaise (Strict.StateT s m) where
  type Errors (Strict.StateT s m) = Errors m
  raise = lift . raise

instance (MonadRaise m, Monoid w) => MonadRaise (Lazy.WriterT w m) where
  type Errors (Lazy.WriterT w m) = Errors m
  raise = lift . raise

instance (MonadRaise m, Monoid w) => MonadRaise (Strict.WriterT w m) where
  type Errors (Strict.WriterT w m) = Errors m
  raise = lift . raise

instance (MonadRaise m, Monoid w) => MonadRaise (Lazy.RWST r w s m) where
  type Errors (Lazy.RWST r w s m) = Errors m
  raise = lift . raise

instance (MonadRaise m, Monoid w) => MonadRaise (Strict.RWST r w s m) where
  type Errors (Strict.RWST r w s m) = Errors m
  raise = lift . raise

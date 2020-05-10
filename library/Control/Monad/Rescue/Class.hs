{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The 'MonadRescue' class

module Control.Monad.Rescue.Class (MonadRescue (..)) where

import           Data.WorldPeace

import           Control.Monad.Raise

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

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
--
-- >>> import Control.Monad.Trans.Rescue
-- >>> import Data.Proxy
-- >>> import Data.WorldPeace as OpenUnion
--
-- >>> data FooErr  = FooErr  deriving Show
-- >>> data BarErr  = BarErr  deriving Show
-- >>> data QuuxErr = QuuxErr deriving Show

-- | Pull a potential error out of the surrounding context
class MonadRaise (OpenUnion errs) m => MonadRescue errs m where
-- class Monad m => MonadRescue errs m where
  -- | Attempt some action, exposing the success and error branches
  -- 
  --  The @Proxy@ gives a type hint to the type checker.
  --  If you have a case where it can be inferred, see 'Control.Monad.Rescue.try''.
  --
  --  ==== __Examples__ 
  --
  --  >>> type MyErrs = '[FooErr, BarErr]
  --  >>> myErrs = Proxy @MyErrs
  --
  --  >>> :{
  --    goesBoom :: Int -> Rescue MyErrs Int
  --    goesBoom x =
  --      if x > 50
  --        then return x
  --        else raise @MyErrs FooErr
  -- :}
  --
  -- >>> runRescue . try myErrs $ goesBoom 42
  -- Right (Left (Identity FooErr))
  --
  -- Where @Identity fooErr@ is the selection of the 'OpenUnion'.
  -- In practice you would handle the 'OpenUnion' like so:
  --
  -- >>> let handleErr = catchesOpenUnion (show, show)
  -- >>> let x = try myErrs (goesBoom 42) >>= pure . either handleErr show
  -- >>> runRescue x
  -- Right "FooErr"
  try :: m a -> m (Either (OpenUnion errs) a) -- FIXME rename attempt

instance MonadRescue errs (Either (OpenUnion errs)) where
  try action = Right action

instance MonadRescue errs m => MonadRescue errs (MaybeT m) where
  try (MaybeT action) = MaybeT . fmap sequence $ try action

instance MonadRescue errs m => MonadRescue errs (IdentityT m) where
  try (IdentityT action) = IdentityT (try action)

instance MonadRescue errs m => MonadRescue errs (ExceptT (OpenUnion errs) m) where
  try (ExceptT action) = ExceptT (try action)

instance MonadRescue errs m => MonadRescue errs (ReaderT cfg m) where
  try (ReaderT action) = ReaderT $ \cfg -> try (action cfg)

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Lazy.WriterT w m) where
  try = Lazy.WriterT . Lazy.runWriterT . try

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Strict.WriterT w m) where
  try = Strict.WriterT . Strict.runWriterT . try

instance MonadRescue errs m => MonadRescue errs (Lazy.StateT s m) where
  try = Lazy.StateT . Lazy.runStateT . try

instance MonadRescue errs m => MonadRescue errs (Strict.StateT s m) where
  try = Strict.StateT . Strict.runStateT . try

instance MonadRescue errs m => MonadRescue errs (ContT r m) where
  try = ContT . runContT . try

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Lazy.RWST r w s m) where
  try = Lazy.RWST . Lazy.runRWST . try

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Strict.RWST r w s m) where
  try = Strict.RWST . Strict.runRWST . try

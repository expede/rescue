{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The 'MonadRescue' class

module Control.Monad.Rescue.Class (MonadRescue (..)) where

import           Data.Proxy
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
class MonadRaise errs m => MonadRescue errs m where
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
  --        else raiseAs @MyErrs FooErr
  -- :}
  --
  -- >>> runRescue $ try myErrs $ goesBoom 42
  -- Right (Left (Identity FooErr))
  --
  -- Where @Identity fooErr@ is the selection of the 'OpenUnion'.
  -- In practice you would handle the 'OpenUnion' like so:
  --
  -- >>> let handleErr = catchesOpenUnion (show, show)
  -- >>> let x = try myErrs (goesBoom 42) >>= pure . either handleErr show
  -- >>> runRescue x
  -- Right "FooErr"
  try :: Proxy errs -> m a -> m (Either (OpenUnion errs) a)

instance MonadRescue errs (Either (OpenUnion errs)) where
  try _ = Right

instance MonadRescue errs m => MonadRescue errs (MaybeT m) where
  try pxy (MaybeT action) = MaybeT . fmap sequence $ try pxy action

instance MonadRescue errs m => MonadRescue errs (IdentityT m) where
  try pxy (IdentityT action) = IdentityT (try pxy action)

instance MonadRescue errs m => MonadRescue errs (ExceptT (OpenUnion errs) m) where
  try pxy (ExceptT action) = ExceptT (try pxy action)

instance MonadRescue errs m => MonadRescue errs (ReaderT cfg m) where
  try pxy (ReaderT action) = ReaderT $ \cfg -> try pxy (action cfg)

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Lazy.WriterT w m) where
  try pxy = Lazy.WriterT . Lazy.runWriterT . try pxy

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Strict.WriterT w m) where
  try pxy = Strict.WriterT . Strict.runWriterT . try pxy

instance MonadRescue errs m => MonadRescue errs (Lazy.StateT s m) where
  try pxy = Lazy.StateT . Lazy.runStateT . try pxy

instance MonadRescue errs m => MonadRescue errs (Strict.StateT s m) where
  try pxy = Strict.StateT . Strict.runStateT . try pxy

instance MonadRescue errs m => MonadRescue errs (ContT r m) where
  try pxy = ContT . runContT . try pxy

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Lazy.RWST r w s m) where
  try pxy = Lazy.RWST . Lazy.runRWST . try pxy

instance (Monoid w, MonadRescue errs m) => MonadRescue errs (Strict.RWST r w s m) where
  try pxy = Strict.RWST . Strict.runRWST . try pxy

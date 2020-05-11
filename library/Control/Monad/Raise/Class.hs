-- {-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
-- {-# LANGUAGE UndecidableInstances  #-}




{-# LANGUAGE FunctionalDependencies #-}

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

import Data.Proxy
import           Data.WorldPeace

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
--
-- >>> import Data.WorldPeace

-- | Raise semantics, like a type-directed @MonadThrow@
class Monad m => MonadRaise errs m | m -> errs where
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
  raise :: IsMember err errs => err -> m a

-- instance MonadRaise errs [] where
--   raise _ = []

-- instance MonadRaise errs Maybe where
--   raise _ = Nothing

instance MonadRaise errs (Either (OpenUnion errs)) where
  raise = Left . openUnionLift

instance Monad m => MonadRaise errs (ExceptT (OpenUnion errs) m) where
  raise err = ExceptT . pure $ raise err -- Left . openUnionLift

instance MonadRaise errs m => MonadRaise errs (IdentityT m) where
  raise = lift . raise

instance MonadRaise errs m => MonadRaise errs (MaybeT m) where
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

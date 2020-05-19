{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | The 'MonadRescue' class FIXME expand text

module Control.Monad.Rescue.Class (MonadRescue (..)) where

import           Data.Functor

import           Data.WorldPeace

import           Exception

import           Control.Monad.Raise.Class

import           Control.Monad.Cont

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

import           Data.WorldPeace.Subset.Class

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
class MonadRaise m => MonadRescue m where -- FIXME make a constraint synonym for MonadRaise + MonadRescue
  -- | Attempt some action, exposing the success and error branches
  --
  --  The @Proxy@ gives a type hint to the type checker. -- FIXME
  --  If you have a case where it can be inferred, see 'Control.Monad.Rescue.attempt''.
  --
  --  ==== __Examples__
  --
  --  >>> type MyErrs = '[FooErr, BarErr]
  --  >>> myErrs = Proxy @MyErrs -- FIXME
  --
  --  >>> :{
  --    goesBoom :: Int -> Rescue MyErrs Int
  --    goesBoom x =
  --      if x > 50
  --        then return x
  --        else raise @MyErrs FooErr
  -- :}
  --
  -- >>> runRescue . attempt myErrs $ goesBoom 42
  -- Right (Left (Identity FooErr))
  --
  -- Where @Identity fooErr@ is the selection of the 'OpenUnion'.
  -- In practice you would handle the 'OpenUnion' like so:
  --
  -- >>> let handleErr = catchesOpenUnion (show, show)
  -- >>> let x = attempt myErrs (goesBoom 42) >>= pure . either handleErr show
  -- >>> runRescue x
  -- Right "FooErr"
  attempt :: m a -> m (Either (OpenUnion (Errors m)) a)

-- FIXME
instance MonadRescue IO where
  attempt action =
    tryIO action <&> \case
      Right val  -> Right val
      Left ioExc -> Left $ include ioExc

instance MonadRescue Maybe where
  attempt = return . \case
    Nothing -> Left $ openUnionLift ()
    Just x  -> Right x

instance MonadRescue [] where -- NOTE this is essentially safeHead?
  attempt = return . \case
    []      -> Left $ include ()
    (a : _) -> Right a

instance MonadRescue (Either (OpenUnion errs)) where
  attempt action = Right action

instance MonadRescue m => MonadRescue (MaybeT m) where
  attempt (MaybeT action) = MaybeT . fmap sequence $ attempt action

instance MonadRescue m => MonadRescue (IdentityT m) where
  attempt (IdentityT action) = lift (attempt action)

-- ListT

instance
  ( MonadRescue m
  , Contains errs errs
  , errs ~ Errors m
  )
  => MonadRescue (ExceptT (OpenUnion errs) m) where
    attempt (ExceptT action) = ExceptT $ attempt action

instance MonadRescue m => MonadRescue (ReaderT cfg m) where
  attempt = mapReaderT attempt

instance (Monoid w, MonadRescue m) => MonadRescue (Lazy.WriterT w m) where
  attempt = Lazy.mapWriterT runner2 -- FIXME lok at the MonadCatch instance, it can write the err

   -- catch (LazyW.WriterT m) h = LazyW.WriterT $ m `catch ` \e -> LazyW.runWriterT (h e)

instance (Monoid w, MonadRescue m) => MonadRescue (Strict.WriterT w m) where
  attempt = Strict.mapWriterT runner2

instance MonadRescue m => MonadRescue (Lazy.StateT s m) where
  attempt = Lazy.mapStateT runner2

instance MonadRescue m => MonadRescue (Strict.StateT s m) where
  attempt = Strict.mapStateT runner2

instance (Monoid w, MonadRescue m) => MonadRescue (Lazy.RWST r w s m) where
  attempt = Lazy.mapRWST runner3

instance (Monoid w, MonadRescue m) => MonadRescue (Strict.RWST r w s m) where
  attempt = Strict.mapRWST runner3

instance MonadRescue m => MonadRescue (ContT r m) where
  attempt = withContT $ \b_mr current -> b_mr =<< attempt (pure current)

-- instamce STM

runner2
  :: ( MonadRescue m
     , errs ~ Errors m
     )
  => m (a, w)
  -> m (Either (OpenUnion errs) a, w)
runner2 inner = do
  (a, w)   <- inner
  errOrVal <- attempt (pure a)
  return (errOrVal, w)

runner3
  :: ( MonadRescue m
     , errs ~ Errors m
     )
  => m (a, b, c)
  -> m (Either (OpenUnion errs) a, b, c)
runner3 inner = do
  (a, s, w) <- inner
  errOrVal  <- attempt (pure a)
  return (errOrVal, s, w)

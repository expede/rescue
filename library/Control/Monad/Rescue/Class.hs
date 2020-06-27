{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MagicHash              #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | The 'MonadRescue' class FIXME expand text

module Control.Monad.Rescue.Class (MonadRescue (..)) where

-- import           GHC.Conc
-- import           GHC.Exts

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

-- TODO Make a comment about attempt (raise err) == pure (Left err)

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
  --  >>> :{
  --    goesBoom :: Int -> Rescue '[FooErr, BarErr] Int
  --    goesBoom x =
  --      if x > 50
  --        then return x
  --        else raise FooErr
  -- :}
  --
  -- >>> runRescue . attempt $ goesBoom 42
  -- Right (Left (Identity FooErr))
  --
  -- Where @Identity fooErr@ is the selection of the 'OpenUnion'.
  -- In practice you would handle the 'OpenUnion' like so:
  --
  -- >>> let handleErr = catchesOpenUnion (show, show)
  -- >>> let x = attempt (goesBoom 42) >>= pure . either handleErr show
  -- >>> runRescue x
  -- Right "FooErr"
  attempt :: m a -> m (Either (OpenUnion (Errors m)) a)

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

-- FIXME
instance MonadRescue IO where
  attempt action =
    tryIO action <&> \case
      Right val  -> Right val
      Left ioExc -> Left $ include ioExc

-- instance MonadRescue (ST s) where
--   attempt action =

-- instance MonadRescue STM where
--   attempt (STM action) = STM $ catchSTM# undefined handler -- action undefined -- handler
--     where
--       handler err state = (state, Left (include err))
--       unSTM' (STM a) = a

{-
catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
catchSTM (STM m) handler = STM $ catchSTM# m handler'
    where
      handler' e = case fromException e of
                     Just e' -> unSTM (handler e')
                     Nothing -> raiseIO# e
-}

instance MonadRescue m => MonadRescue (MaybeT m) where
  attempt (MaybeT action) = MaybeT . fmap sequence $ attempt action

instance MonadRescue m => MonadRescue (IdentityT m) where
  attempt (IdentityT action) = lift (attempt action)

-- ListT

-- NOTE type constrained because of the method signature
-- This means that while the Raise doesn't (yet?) require `Contains (Errors m) errs`
--   to rescue the outer errs, you need to have it as a subset of the Either's `errs`
-- FIXME may want to add the Contains to `MonadRaise ExceptT`
instance
  ( MonadRescue m
  , Contains (Errors m) errs
  )
  => MonadRescue (ExceptT (OpenUnion errs) m) where
  attempt (ExceptT action) =
    ExceptT $ attempt action <&> \case
      Left err       -> Left $ include err
      Right errOrVal -> Right errOrVal

instance MonadRescue m => MonadRescue (ReaderT cfg m) where
  attempt = mapReaderT attempt

instance (Monoid w, MonadRescue m) => MonadRescue (Lazy.WriterT w m) where
  attempt = Lazy.mapWriterT runner2

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

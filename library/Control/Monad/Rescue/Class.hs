{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The 'MonadRescue' class, meant for retrieving the success/failure branches

module Control.Monad.Rescue.Class (MonadRescue (..)) where

import           Data.WorldPeace

import           Control.Exception

import qualified Control.Monad.Catch          as Catch
import           Control.Monad.Cont

import           Control.Monad.Raise

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

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
-- >>> :set -XLambdaCase
--
-- >>> import Control.Monad.Trans.Rescue
-- >>> import Data.Functor.Identity
-- >>> import Data.Proxy
-- >>> import Data.WorldPeace as OpenUnion
--
-- >>> data FooErr  = FooErr  deriving Show
-- >>> data BarErr  = BarErr  deriving Show
-- >>> data QuuxErr = QuuxErr deriving Show

-- | Pull a potential error out of the surrounding context
-- NOTE that the target `m` may not even be aware of Raise/Rescue. It's an escape to the "normal" world
class MonadRaise m => MonadRescue m where
  -- | Attempt some action, exposing the success and error branches
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
  --
  -- Where @Identity FooErr@ is the selection of the 'OpenUnion'.
  attempt :: m a -> m (Either (ErrorCase m) a)

instance MonadRescue Maybe where
  attempt Nothing  = Just . Left $ openUnionLift ()
  attempt (Just x) = Just $ Right x

instance MonadRescue [] where
  attempt [] = [Left $ include ()]
  attempt xs = Right <$> xs

instance MonadRescue (Either (OpenUnion errs)) where
  attempt action = Right action

instance MonadRescue IO where
  attempt action =
    Catch.try action >>= \case
      Left (err :: IOException) -> return . Left $ include err
      Right val                 -> return $ Right val

instance
  ( MonadRescue m
  , () `IsMember` Errors m
  , Errors m `Contains` Errors m
  )
  => MonadRescue (MaybeT m) where
  attempt (MaybeT action) =
    MaybeT $
      attempt action >>= \case
        Left errs        -> return . Just . Left $ include errs
        Right Nothing    -> return . Just . Left $ include ()
        Right (Just val) -> return . Just $ Right val

instance MonadRescue m => MonadRescue (IdentityT m) where
  attempt (IdentityT action) = IdentityT $ attempt action

instance
  ( MonadRescue m
  , Contains (Errors m) errs
  )
  => MonadRescue (ExceptT (OpenUnion errs) m) where
  attempt (ExceptT action) =
    lift $
      attempt action >>= \case
        Left err       -> return . Left $ include err
        Right errOrVal -> return errOrVal

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
     , RaisesOnly  m errs
     )
  => m (a, w)
  -> m (Either (OpenUnion errs) a, w)
runner2 inner = do
  (a, w)   <- inner
  errOrVal <- attempt (pure a)
  return (errOrVal, w)

runner3
  :: ( MonadRescue m
     , RaisesOnly  m errs
     )
  => m (a, b, c)
  -> m (Either (OpenUnion errs) a, b, c)
runner3 inner = do
  (a, s, w) <- inner
  errOrVal  <- attempt (pure a)
  return (errOrVal, s, w)

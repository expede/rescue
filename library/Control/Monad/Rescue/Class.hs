{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The 'MonadRescue' class, meant for retrieving the success/failure branches

module Control.Monad.Rescue.Class (MonadRescueFrom (..)) where

import           Data.Functor
import           Data.WorldPeace

import           Exception

import           Control.Monad.Base
import           Control.Monad.Cont

import           Control.Monad.Raise.Class
import           Control.Monad.Raise.Constraint

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import qualified Control.Monad.RWS.Lazy         as Lazy
import qualified Control.Monad.RWS.Strict       as Strict

import qualified Control.Monad.State.Lazy       as Lazy
import qualified Control.Monad.State.Strict     as Strict

import qualified Control.Monad.Writer.Lazy      as Lazy
import qualified Control.Monad.Writer.Strict    as Strict

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
  -- FIXME applicative?
  -- NOTE that the target `n` may not even be aware of Raise/Rescue. It's an escape to the "normal" world
class (Monad m, MonadRaise n) => MonadRescueFrom n m where
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
  attempt :: n a -> m (Either (OpenUnion (Errors n)) a)

instance Monad n => MonadRescueFrom Maybe n where
  attempt = return . \case
    Nothing -> Left $ openUnionLift ()
    Just x  -> Right x

instance Monad m => MonadRescueFrom [] m where
  attempt = return . \case
    []      -> Left $ include ()
    (a : _) -> Right a

instance Monad m => MonadRescueFrom (Either (OpenUnion errs)) m where
  attempt action = pure action

instance MonadIO m => MonadRescueFrom IO m where
  attempt action =
    liftIO (tryIO action) <&> \case
      Right val  -> Right val
      Left ioExc -> Left $ include ioExc

instance
  ( IsMember () (Errors n) -- FIXME why not automagicly added my MaybeT?
    -- n `Raises` () -- FIXME think about this again, please
  , Monad m
  , MonadRescueFrom n m
  )
  => MonadRescueFrom (MaybeT n) m where
    attempt (MaybeT action) =
      attempt action <&> \case
        Right (Just val) -> Right val
        Right Nothing    -> Left $ include ()
        Left errs        -> Left errs

instance MonadRescueFrom n m => MonadRescueFrom (IdentityT n) m where
  attempt (IdentityT action) = attempt action

instance
  ( MonadRescueFrom n m
  , Contains (Errors n) errs
  )
  => MonadRescueFrom (ExceptT (OpenUnion errs) n) m where
  attempt (ExceptT action) =
    attempt action <&> \case
      Left err       -> Left $ include err
      Right errOrVal -> errOrVal

instance
  ( Monad               m
  , MonadBase       n   m
  , MonadRaise      n
  , MonadRescueFrom n m
  )
  => MonadRescueFrom (ReaderT cfg n) (ReaderT cfg m) where
    attempt = mapReaderT attempt

instance
  ( Monad               m
  , MonadBase       n   m
  , MonadRaise      n
  , MonadRescueFrom n n
  )
  => MonadRescueFrom n (ReaderT cfg m) where
    attempt = liftBase . attempt

instance
  ( Monoid w
  , MonadBase       n m
  , MonadRescueFrom n m
  )
  => MonadRescueFrom (Lazy.WriterT w n) (Lazy.WriterT w m) where
    attempt = Lazy.mapWriterT runner2

instance
  ( Monoid w
  , MonadBase       n m
  , MonadRescueFrom n n
  )
  => MonadRescueFrom n (Lazy.WriterT w m) where
    attempt = liftBase . attempt

instance
  ( Monoid w
  , MonadBase       n m
  , MonadRescueFrom n m
  )
  => MonadRescueFrom (Strict.WriterT w n) (Strict.WriterT w m) where
    attempt = Strict.mapWriterT runner2

instance
  ( Monoid w
  , MonadBase       n m
  , MonadRescueFrom n n
  )
  => MonadRescueFrom n (Strict.WriterT w m) where
    attempt = liftBase . attempt

instance
  ( MonadBase       n m
  , MonadRescueFrom n m
  )
  => MonadRescueFrom (Lazy.StateT s n) (Lazy.StateT s m) where
    attempt = Lazy.mapStateT runner2

instance
  ( MonadBase       n m
  , MonadRescueFrom n n
  )
  => MonadRescueFrom n (Lazy.StateT s m) where
    attempt = liftBase . attempt

instance
  ( MonadBase       n m
  , MonadRescueFrom n m
  )
  => MonadRescueFrom (Strict.StateT s n) (Strict.StateT s m) where
    attempt = Strict.mapStateT runner2

instance
  ( Monoid w
  , MonadBase n m
  , MonadRescueFrom n m
  )
  => MonadRescueFrom (Lazy.RWST r w s n) (Lazy.RWST r w s m) where
    attempt = Lazy.mapRWST runner3

instance
  ( MonadBase       n m
  , MonadRescueFrom n n
  )
  => MonadRescueFrom n (Strict.StateT s m) where
    attempt = liftBase . attempt

instance
  ( Monoid w
  , MonadBase       n m
  , MonadRescueFrom n n
  )
  => MonadRescueFrom n (Strict.RWST r w s m) where
    attempt = liftBase . attempt

instance
  ( MonadBase       n m
  , MonadRescueFrom n n
  )
  => MonadRescueFrom n (ContT r m) where
    attempt = liftBase . attempt

instance forall m r . (MonadRescueFrom (ContT r m) m) => MonadRescueFrom (ContT r m) (ContT r m) where
  attempt =
    withContT $ \b_mr (current :: a) ->
      b_mr =<< attempt (pure current :: ContT r m a)

runner2
  :: forall m n errs a w .
     ( MonadBase n m
     , MonadRescueFrom n m
     , n `RaisesOnly` errs
     )
  => n (a, w)
  -> m (Either (OpenUnion (Errors n)) a, w)
runner2 inner = do
  (val, log')  <- liftBase inner
  result <- attempt (pure val :: n a)
  return (result, log')

runner3
  :: forall m n errs a s w .
     ( MonadBase       n m
     , MonadRescueFrom n m
     , n `RaisesOnly` errs
     )
  => n (a, s, w)
  -> m (Either (OpenUnion errs) a, s, w)
runner3 inner = do
  (val, state, log') <- liftBase inner
  result             <- attempt (pure val :: n a)
  return (result, state, log')

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | The 'MonadRescue' class, meant for retrieving the success/failure branches

module Control.Monad.Rescue.Class (MonadRescueTo (..)) where

import           Data.Functor

import           Data.WorldPeace

import           Exception

import           Control.Monad.Raise.Class
import           Control.Monad.Raise.Constraint

import           Control.Monad.Cont

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
class (Monad n, MonadRaise m) => MonadRescueTo m n where
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
  attempt :: m a -> n (Either (OpenUnion (Errors m)) a)

instance Monad n => MonadRescueTo Maybe n where
  attempt = return . \case
    Nothing -> Left $ openUnionLift ()
    Just x  -> Right x

instance Monad m => MonadRescueTo [] m where
  attempt = return . \case
    []      -> Left $ include ()
    (a : _) -> Right a

instance Monad m => MonadRescueTo (Either (OpenUnion errs)) m where
  attempt action = pure action

instance MonadIO n => MonadRescueTo IO n where
  attempt action =
    liftIO (tryIO action) <&> \case
      Right val  -> Right val
      Left ioExc -> Left $ include ioExc

instance
  ( IsMember () (Errors m)
  , Monad n
  , MonadRescueTo m n
  )
  => MonadRescueTo (MaybeT m) n where
    attempt (MaybeT action) =
      attempt action <&> \case
        Right (Just val) -> Right val
        Right Nothing    -> Left $ include ()
        Left errs        -> Left errs

instance MonadRescueTo m n => MonadRescueTo (IdentityT m) n where
  attempt (IdentityT action) = attempt action

instance
  ( MonadRescueTo m n
  , Contains (Errors m) errs
  )
  => MonadRescueTo (ExceptT (OpenUnion errs) m) n where
  attempt (ExceptT action) =
    attempt action <&> \case
      Left err       -> Left $ include err
      Right errOrVal -> errOrVal

-- instance (Monad n, MonadRescueTo m n) => MonadRescueTo (ReaderT cfg m) n where
  -- attempt = mapReaderT attempt

-- instance (Monoid w, MonadRescueTo m n) => MonadRescueTo (Lazy.WriterT w m) n where
  -- attempt = Lazy.mapWriterT runner2

-- instance (Monoid w, MonadRescueTo m n) => MonadRescueTo (Strict.WriterT w m) n where
  -- attempt = Strict.mapWriterT runner2

-- instance MonadRescueTo m n => MonadRescueTo (Lazy.StateT s m) n where
  -- attempt = Lazy.mapStateT runner2

-- instance MonadRescueTo m n => MonadRescueTo (Strict.StateT s m) n where
  -- attempt = Strict.mapStateT runner2

-- instance (Monoid w, MonadRescueTo m n) => MonadRescueTo (Lazy.RWST r w s m) n where
  -- attempt = Lazy.mapRWST runner3

-- instance (Monoid w, MonadRescueTo m n) => MonadRescueTo (Strict.RWST r w s m) n where
  -- attempt = Strict.mapRWST runner3

-- instance MonadRescueTo m n => MonadRescueTo (ContT r m) n where
  -- attempt = withContT $ \b_mr current -> b_mr =<< attempt (pure current)
--
-- runner2
--   :: ( Monad           n
--      , MonadRescueTo m n
--      , RaisesOnly    m errs
--      )
--   => m (a, w)
--   -> n (Either (OpenUnion errs) a, w)
-- runner2 inner = do
--   errOrVal <- attempt (pure a)
--   return (errOrVal, w)
--   where
--     foo = do
--       (a, w) <- inner
--       return (a, w)

-- runner2
--   :: ( Monad           n
--      , MonadRescueTo m n
--      , RaisesOnly    m errs
--      )
--   => m (a, w)
--   -> n (Either (OpenUnion errs) a, w)
-- runner2 inner = do
--   (a, w)   <- inner
--   errOrVal <- attempt (pure a)
--   return (errOrVal, w)

-- runner3
--   :: ( MonadRescueTo m n
--      , RaisesOnly  m errs
--      )
--   => m (a, b, c)
--   -> n (Either (OpenUnion errs) a, b, c)
-- runner3 inner = do
--   (a, s, w) <- inner
--   errOrVal  <- attempt (pure a)
--   return (errOrVal, s, w)

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

import           Data.Functor
import           Data.WorldPeace

import           Control.Exception

import           Control.Monad.Base
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
  -- >>> :{
  --   result :: Identity (Either (OpenUnion '[FooErr, BarErr]) Int)
  --   result = attempt $ goesBoom 42
  -- :}
  --
  -- >>> result
  -- Identity (Left (Identity FooErr))
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

-- instance MonadRescue m => MonadRescue (IdentityT m) where
  -- attempt (IdentityT action) = attempt action

-- instance
--   ( MonadBase       n m
--   , MonadRescue n n
--   , Contains (Errors n) errs
--   )
--   => MonadRescue n (ExceptT (OpenUnion errs) m) where
--     attempt = liftBase . attempt
--
-- instance
--   ( Monad             m
--   , MonadBase       n m
--   , MonadRaise      n
--   , MonadRescue n m
--   )
--   => MonadRescue (ReaderT cfg n) (ReaderT cfg m) where
--     attempt = mapReaderT attempt
--
-- instance
--   ( Monad               m
--   , MonadBase       n   m
--   , MonadRaise      n
--   , MonadRescue n n
--   )
--   => MonadRescue n (ReaderT cfg m) where
--     attempt = liftBase . attempt
--
-- instance
--   ( Monoid w
--   , MonadBase       n m
--   , MonadRescue n m
--   )
--   => MonadRescue (Lazy.WriterT w n) (Lazy.WriterT w m) where
--     attempt = Lazy.mapWriterT runner2
--
-- instance
--   ( Monoid w
--   , MonadBase       n m
--   , MonadRescue n n
--   )
--   => MonadRescue n (Lazy.WriterT w m) where
--     attempt = liftBase . attempt
--
-- instance
--   ( Monoid w
--   , MonadBase       n m
--   , MonadRescue n m
--   )
--   => MonadRescue (Strict.WriterT w n) (Strict.WriterT w m) where
--     attempt = Strict.mapWriterT runner2
--
-- instance
--   ( Monoid w
--   , MonadBase       n m
--   , MonadRescue n n
--   )
--   => MonadRescue n (Strict.WriterT w m) where
--     attempt = liftBase . attempt
--
-- instance
--   ( MonadBase       n m
--   , MonadRescue n m
--   )
--   => MonadRescue (Lazy.StateT s n) (Lazy.StateT s m) where
--     attempt = Lazy.mapStateT runner2
--
-- instance
--   ( MonadBase       n m
--   , MonadRescue n n
--   )
--   => MonadRescue n (Lazy.StateT s m) where
--     attempt = liftBase . attempt
--
-- instance
--   ( MonadBase       n m
--   , MonadRescue n m
--   )
--   => MonadRescue (Strict.StateT s n) (Strict.StateT s m) where
--     attempt = Strict.mapStateT runner2
--
-- instance
--   ( Monoid w
--   , MonadBase n m
--   , MonadRescue n m
--   )
--   => MonadRescue (Lazy.RWST r w s n) (Lazy.RWST r w s m) where
--     attempt = Lazy.mapRWST runner3
--
-- instance
--   ( MonadBase       n m
--   , MonadRescue n n
--   )
--   => MonadRescue n (Strict.StateT s m) where
--     attempt = liftBase . attempt
--
-- instance
--   ( Monoid w
--   , MonadBase   n m
--   , MonadRescue n n
--   )
--   => MonadRescue n (Strict.RWST r w s m) where
--     attempt = liftBase . attempt
--
-- instance
--   ( MonadBase       n m
--   , MonadRescue n n
--   )
--   => MonadRescue n (ContT r m) where
--     attempt = liftBase . attempt
--
-- instance forall m r . (MonadRescue (ContT r m) m) => MonadRescueFrom (ContT r m) (ContT r m) where
--   attempt =
--     withContT $ \b_mr (current :: a) ->
--       b_mr =<< attempt (pure current :: ContT r m a)
--
-- runner2
--   :: forall m n errs a w .
--      ( MonadBase n m
--      , MonadRescue n m
--      , n `RaisesOnly` errs
--      )
--   => n (a, w)
--   -> m (Either (ErrorCase n) a, w)
-- runner2 inner = do
--   (val, log')  <- liftBase inner
--   result <- attempt (pure val :: n a)
--   return (result, log')
--
-- runner3
--   :: forall m n errs a s w .
--      ( MonadBase       n m
--      , MonadRescue n m
--      , n `RaisesOnly` errs
--      )
--   => n (a, s, w)
--   -> m (Either (OpenUnion errs) a, s, w)
-- runner3 inner = do
--   (val, state, log') <- liftBase inner
--   result             <- attempt (pure val :: n a)
--   return (result, state, log')

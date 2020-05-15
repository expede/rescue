{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}




{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}





{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}





-- | FIXME

module Control.Monad.Raise.Class
  ( MonadRaise (..)
  -- FIXME move to own module
  , Convert (..)
  -- Probably cut later
  , Converty
  , Convert' (..)
  ) where

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
import Data.Kind
import           Data.WorldPeace

type family (Converty a) :: Bool where
  Converty (OpenUnion a) = 'True
  Converty a             = 'False

-- NOTE flag :: Bool is a hack around the overlapping instances problem

-- FIXME rename and move to own module
class Convert' (flag :: Bool) (err :: Type) (errs :: [Type]) where
  convert' :: Proxy flag -> err -> OpenUnion errs

instance Contains err errs => Convert' 'True (OpenUnion err) errs where
  convert' _ = relaxOpenUnion

instance IsMember err errs => Convert' 'False err errs where
  convert' _ = openUnionLift

class Convert err errs where
  convert :: err -> errs

instance (Converty err ~ flag, Convert' flag err errs) => Convert err (OpenUnion errs) where
  convert = convert' (Proxy @flag)

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
  raise :: Convert err (OpenUnion (Errors m)) => err -> m a

-- FIXME move to own module

--   -- NOTE NOT POSSIBLE
-- instance MonadRaise [] where
--   type Errors [] = '[] -- FIXME maybe this?
--   raise (IndexOutOfBounds str) = []

-- data NotFound a
--   = NotFound

-- instance MonadRaise Maybe where
--   type Errors Maybe = '[NotFound ()] -- hmmm right does not depend on the `a`
--   raise NotFound = Nothing

--

instance MonadRaise Maybe where
  type Errors Maybe = '[()] -- Seems bad somehow
  raise _ = Nothing -- FIXME test this

-- NOTE can be aliased as `MonadRaise (Result errs)`
instance MonadRaise (Either (OpenUnion errs)) where
  type Errors (Either (OpenUnion errs)) = errs
  raise = Left . convert

instance Monad m => MonadRaise (ExceptT (OpenUnion errs) m) where
  type Errors (ExceptT (OpenUnion errs) m) = errs
  raise = except . raise -- NOTE replaces/same as `throwE`

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

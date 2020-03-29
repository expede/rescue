{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |

module Data.Ensure
  ( EnsureT (..)
  , Ensure
  , runEnsure
  ) where

import           Control.Monad.Raise
import           Control.Monad.Rescue

import           Control.Monad.Trans
import           Control.Monad.Trans.Except

import           Data.Functor.Identity (Identity)
import           Data.WorldPeace

type EnsureT errs = ExceptT (OpenUnion errs)

type Ensure errs = Except (OpenUnion errs)

-- newtype EnsureT errs m a
--   = EnsureT { runEnsureT :: ExceptT (OpenUnion errs) m a }
--   deriving ( Foldable
--            , Functor
--            , Applicative
--            , Monad
--            , MonadTrans
--            , MonadRaise errs
--            , MonadRescue errs
--            )

-- type Ensure errs
--   = EnsureT errs Identity

runEnsureT :: EnsureT errs m a -> m (Either (OpenUnion errs) a)
runEnsureT = runExceptT

runEnsure :: Ensure errs a -> (Either (OpenUnion errs) a)
runEnsure = runExcept
-- runEnsure = runEnsureT


-- mapEnsureT ::
--     (ExceptT (OpenUnion errs) m a -> ExceptT (OpenUnion errs) n b)
--   -> EnsureT errs m a
--   -> EnsureT errs n b
-- mapEnsureT f (EnsureT action) = EnsureT $ mapExceptT f action

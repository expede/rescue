{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Monadic raise semantics

module Control.Monad.Raise
  ( module Control.Monad.Raise.Class
  , raiseAs
  , raiseTo
 
  , ensure
  , ensureM

  , ensure1
  , ensureM1
  ) where

import           Control.Monad.Raise.Class

import           Data.Proxy
import           Data.WorldPeace

  -- Not Raise-related, just helpers

liftAs :: IsMember err errs => Proxy errs -> err -> OpenUnion errs
liftAs _proxy = openUnionLift

liftTo :: Contains inner outer => Proxy outer -> OpenUnion inner -> OpenUnion outer
liftTo _proxy = relaxOpenUnion

------------------

raiseAs :: IsMember err errs => MonadRaise errs m => Proxy errs -> err -> m a
raiseAs proxy = raise . liftAs proxy

raiseTo :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> OpenUnion inner
  -> m a
raiseTo proxy = raise . liftTo proxy

---------------------

ensure1 :: forall m a err errs.
  (IsMember err errs, MonadRaise errs m) => Either err a -> m a
ensure1 = either (raiseAs (Proxy @errs)) pure

ensureM1 :: forall err errs m a .
  (IsMember err errs, MonadRaise errs m) => m (Either err a) -> m a
ensureM1 action = either (raiseAs (Proxy @errs)) pure =<< action

ensure :: forall m a inner outer .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Either (OpenUnion inner) a
  -> m a
ensure = either (raiseTo (Proxy @outer)) pure

ensureM :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => m (Either (OpenUnion inner) a)
  -> m a
ensureM action = either (raiseTo (Proxy @outer)) pure =<< action

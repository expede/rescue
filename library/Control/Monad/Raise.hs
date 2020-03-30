{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Monadic raise semantics

module Control.Monad.Raise
  ( module Control.Monad.Raise.Class
  , raise
 
  , raiseAs
  , raiseAs'

  , raiseTo
  , raiseTo'
 
  , ensure
  , ensureM

  , ensure'
  , ensureM'

  , ensure1
  , ensureM1
  ) where

import           Control.Monad.Raise.Class

import           Data.Proxy
import           Data.WorldPeace
 
import           Rescue.Internal.Data.WorldPeace

-- | Raise an error
--
--   A specialized version of @raise'@, which implies the error open union
raise :: forall errs m a . MonadRaise errs m => OpenUnion errs -> m a
raise = raise' (Proxy @errs)

-- | Raise a single error into a particular context
raiseAs' :: (IsMember err errs, MonadRaise errs m) => Proxy errs -> err -> m a
raiseAs' proxy = raise . liftAs proxy

raiseAs :: forall err errs m a .
  (IsMember err errs, MonadRaise errs m) => err -> m a
raiseAs = raiseAs' (Proxy @errs)

raiseTo' :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> OpenUnion inner
  -> m a
raiseTo' proxy = raise . relaxTo proxy

raiseTo :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => OpenUnion inner
  -> m a
raiseTo = raiseTo' (Proxy @outer)

ensure1 :: forall m a err errs.
  (IsMember err errs, MonadRaise errs m) => Either err a -> m a
ensure1 = either (raiseAs' (Proxy @errs)) pure

ensureM1 :: forall err errs m a .
  (IsMember err errs, MonadRaise errs m) => m (Either err a) -> m a
ensureM1 action = either (raiseAs' (Proxy @errs)) pure =<< action

ensure' :: forall m a inner outer .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> Either (OpenUnion inner) a
  -> m a
ensure' pxy = either (raiseTo' pxy) pure

ensure :: forall m a inner outer .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Either (OpenUnion inner) a
  -> m a
ensure = ensure' (Proxy @outer)

ensureM' :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => Proxy outer
  -> m (Either (OpenUnion inner) a)
  -> m a
ensureM' pxy action = either (raiseTo' pxy) pure =<< action

ensureM :: forall inner outer m a .
  ( Contains inner outer
  , MonadRaise outer m
  )
  => m (Either (OpenUnion inner) a)
  -> m a
ensureM = ensureM' (Proxy @outer)

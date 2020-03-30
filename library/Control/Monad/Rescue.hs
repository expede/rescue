{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Control.Monad.Rescue
  ( module Control.Monad.Rescue.Class
  , try
 
  , rescue
  , rescueWith
 
  , reraise
  , handle
  , handleOne

  , cleanup
  , finally
  ) where

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class
 
import           Data.Proxy
import           Data.WorldPeace

try :: forall m a errs . MonadRescue errs m => m a -> m (Either (OpenUnion errs) a)
try = try' (Proxy @errs)

reraise :: forall inner outer m a .
  ( Contains    inner outer
  , MonadRescue inner       m
  , MonadRaise        outer m
  )
  => m a
  -> m a
reraise action = ensureM (Proxy @outer) $ try' (Proxy @inner) action

rescue :: forall m a b errs .
  MonadRescue errs m
  => m a
  -> (Either (OpenUnion errs) a -> m b)
  -> m b
rescue action handler = try' (Proxy @errs) action >>= handler

handle :: MonadRescue errs m => (OpenUnion errs -> m a) -> m a -> m a
handle onErr action = rescue action (either onErr pure)

handleOne :: forall err outer inner m a .
  ( ElemRemove err outer
  , Remove err outer ~ inner
  , MonadRaise  inner m
  , MonadRescue outer m
  )
  => Proxy outer
  -> (err -> m a)
  -> m a
  -> m a
handleOne pxyOuter handler action =
  try' pxyOuter action >>= \case
    Right val -> return val
    Left errs -> openUnionHandle (raise (Proxy @inner)) handler errs

rescueWith ::
  MonadRescue errs m
  => (OpenUnion errs -> m b)
  -> (a -> m b)
  -> m a
  -> m b
rescueWith onErr onOk action = either onErr onOk =<< try action

cleanup :: forall inner outer m resource output ignored1 ignored2 .
  ( Contains    inner outer
  , MonadRescue inner       m
  , MonadRaise        outer m
  )
  => m resource                                  -- ^ Acquire resource
  -> (resource -> OpenUnion inner -> m ignored1) -- ^ Cleanup exception case; The exception will be reraised
  -> (resource -> output          -> m ignored2) -- ^ Cleanup happy path
  -> (resource -> m output)                      -- ^ Inner action
  -> m output
cleanup acquire onErr onOk action = do
  resource <- acquire
  try (action resource) >>= \case
    Right val -> do
      _ <- onOk resource val
      return val

    Left err  -> do
      _ <- onErr resource err
      raiseTo (Proxy @outer) err

finally :: forall errs m a b . MonadRescue errs m => m a -> m b -> m a
finally action finalizer =
  try' (Proxy @errs) action >>= \case
    Right val -> do
      _ <- finalizer
      return val

    Left err -> do
      _ <- finalizer
      raise' err

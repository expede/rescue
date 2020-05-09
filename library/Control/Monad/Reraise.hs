{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 

module Control.Monad.Reraise where

import Control.Monad.Rescue
 
import           Data.Proxy
import           Data.WorldPeace

class MonadRaise outer n => MonadReraise outer m n where
  reraise :: Proxy outer -> m a -> n a
  -- ^ TODO: rename function to `relax` or `relaxErr`? `recontextualize`?
  -- ^ TODO: rename class to `MonadCleanup`?

recontextualize :: MonadReraise outer m n => Proxy outer -> m a -> n a
recontextualize = reraise

rectx :: MonadReraise outer m n => Proxy outer -> m a -> n a
rectx = recontextualize

instance MonadRescue errs m => MonadReraise errs m m where
  reraise _ action = action

instance IsMember err errs =>
  MonadReraise errs (Either err) (Either (OpenUnion errs)) where
    reraise pxyErrs = \case
      Left  err -> raiseAs pxyErrs err
      Right val -> return val

instance Contains innerErrs outerErrs =>
  MonadReraise outerErrs (Either (OpenUnion innerErrs)) (Either (OpenUnion outerErrs)) where
    reraise pxyOuterErrs = \case
      Left  err -> raiseTo pxyOuterErrs err
      Right val -> return val

cleanup :: forall outer n m resource output ignored1 ignored2 .
  ( MonadRescue outer m
  , MonadReraise outer n m
  )
  => Proxy outer
  -> n resource                                  -- ^ Acquire resource
  -> (resource -> OpenUnion outer -> m ignored1) -- ^ Cleanup exception case; The exception will be reraised
  -> (resource -> output          -> m ignored2) -- ^ Cleanup happy path
  -> (resource -> m output)                      -- ^ Inner action
  -> m output
cleanup pxyO acquire onErr onOk action = do
  resource <- reraise pxyO acquire
  try pxyO (action resource) >>= \case
    Left err -> do
      _ <- onErr resource err
      raise err

    Right output -> do
      _ <- onOk resource output
      return output

cleanup' :: forall outer n m resource output ignored1 ignored2 .
  ( MonadRescue outer m
  , MonadReraise outer n m
  )
  => n resource                                  -- ^ Acquire resource
  -> (resource -> OpenUnion outer -> m ignored1) -- ^ Cleanup exception case; The exception will be reraised
  -> (resource -> output          -> m ignored2) -- ^ Cleanup happy path
  -> (resource -> m output)                      -- ^ Inner action
  -> m output
cleanup' = cleanup (Proxy @outer)

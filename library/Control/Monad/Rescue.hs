{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds #-}
-- {-# LANGUAGE FlexibleInstances          #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Control.Monad.Rescue
  ( module Control.Monad.Rescue.Class
  , rescue
  , reraise
  , handle
  ) where

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class
import           Data.WorldPeace

type Relaxable innerErrs outerErrs m =
  ( Contains    innerErrs  outerErrs
  , MonadRescue innerErrs            m
  , MonadRaise             outerErrs m
  )

reraise :: forall innerErrs outerErrs m a .
  Relaxable innerErrs outerErrs m
  => m a
  -> m a
reraise action = do
  attempt :: Either (OpenUnion innerErrs) a <- try action
  case attempt of
    Left  err -> raise' ((relaxOpenUnion err) :: OpenUnion outerErrs)
    Right val -> pure val

rescue :: MonadRescue errs m => m a -> (Either (OpenUnion errs) a -> m b) -> m b
rescue action handler = try action >>= handler

handle :: MonadRescue errs m => (OpenUnion errs -> m a) -> m a -> m a
handle onErr = rescueWith onErr pure

rescueWith ::
  MonadRescue errs m
  => (OpenUnion errs -> m b)
  -> (a -> m b)
  -> m a
  -> m b
rescueWith onErr onOk action = either onErr onOk =<< try action

-- cleanup' :: forall innerErrs outerErrs m resource output ignored1 ignored2 .
--   Relaxable innerErrs outerErrs m
--   => m resource                                   -- ^ Acquire resource
--   -> (resource   -> OpenUnion innerErrs -> m ignored1) -- ^ Cleanup exception case; The exception will be reraised
--   -> (resource   -> output              -> m ignored2)  -- ^ Cleanup happy path
--   -> (m resource -> m output)               -- ^ Inner action
--   -> m output
-- cleanup' acquire onErr onOk action = do
--   resource <- acquire
--   try (action resource) >>= \case
--     Right val -> do
--       _ <- onOk resource val
--       return val

--     Left err  -> do
--       _ <- onErr resource err
--       _ <- raise err
--       return ()


  -- try action >>= \case
  --   Left err -> onRaise err
  --   Right val -> onSuccess

-- generalBracket acquire onSuccess onException inner = E.mask $ \restore -> do
--     x <- acquire
--     res1 <- E.try $ restore $ inner x
--     case res1 of
--         Left (e1 :: E.SomeException) -> do
--             -- explicitly ignore exceptions from the cleanup
--             -- action so we keep the original exception
--             E.uninterruptibleMask_ $ fmap (const ()) (onException x e1) `E.catch`
--                 (\(_ :: E.SomeException) -> return ())
--             E.throwIO e1
--         Right y -> do
--             -- Allow exceptions from the onSuccess function to propagate
--             _ <- onSuccess x y
--             return y

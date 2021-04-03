{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Monad.Rescue.Class where

import           Control.Monad.Attempt.Class
import           Control.Monad.Raise.Class

import           Data.Kind
import           Data.Proxy
import           Data.WorldPeace

-- FIXME swap where the instance lives
import           Control.Monad.Trans.Rescue.Types

class MonadAttempt (m (OpenUnion errs)) => MonadRescue m errs where
  rescue ::
    ( errs ~ Remove err outerErrs
    , ElemRemove err outerErrs
    )
    => (err -> m (OpenUnion errs) a)
    -> m (OpenUnion outerErrs) a
    -> m (OpenUnion errs) a

instance Contains errs errs => MonadRescue Either errs where
  rescue handler = \case
    Right val      -> return val
    Left outerErrs -> openUnionHandle raise handler outerErrs

newtype FlippedRescueT m errs a = FlippedRescueT { runFlipped :: RescueT errs m a }

instance
  ( Monad m
  , MonadAttempt (FlippedRescueT m (OpenUnion errs))
  )
  => MonadRescue (FlippedRescueT m) errs where
    rescue handler (FlippedRescueT (RescueT action)) =
      FlippedRescueT $ RescueT $
        action >>= \case
          Left outerErrs -> openUnionHandle (return . Left) (runRescueT . runFlipped . handler) outerErrs
          Right val      -> return $ Right val

-- instance (Monad m, Contains errs errs) => MonadRescue (FlippedRescueT m) errs where
--   rescue handler (FlippedRescueT (RescueT action)) =
--     FlippedRescueT . RescueT $
--       action >>= \case
--         Right val      -> return $ Right val
--         Left outerErrs -> openUnionHandle (return . Left) (runRescueT . runFlipped . handler) outerErrs

-- class MonadAttempt m => MonadRescue m where
--   type Exposed m :: Type -> Type -> Type
--
--   toExposed   :: Contains (Errors m) errs => Proxy errs -> m a -> Exposed m (OpenUnion errs) a
--
--   rescue ::
--     ( Errors m ~ Remove err outerErrs
--     , ElemRemove err outerErrs
--     )
--     => (err -> m a)
--     -> Exposed m (OpenUnion outerErrs) a
--     -> m a
--
-- instance Contains errs errs => MonadRescue (Either (OpenUnion errs)) where
--   type Exposed (Either (OpenUnion errs)) = Either
--
--   toExposed _ = \case
--     Right val -> Right val
--     Left err  -> raise err
--
--   rescue handler = \case
--     Right val      -> return val
--     Left outerErrs -> openUnionHandle raise handler outerErrs
--
-- newtype FlippedRescueT m errs a = FlippedRescueT (RescueT errs m a)
--
-- instance (Monad m, Contains errs errs) => MonadRescue (RescueT (OpenUnion errs) m) where
--   type Exposed (RescueT (OpenUnion errs) m) = FlippedRescueT m
--
--   toExposed :: Proxy outerErrs -> RescueT (OpenUnion outerErrs) m a -> FlippedRescueT m (OpenUnion outerErrs) a
--   toExposed _ action = FlippedRescueT action
--
--   rescue handler (FlippedRescueT (RescueT action)) =
--     RescueT $
--       action >>= \case
--         Right val      -> return $ Right val
--         Left outerErrs -> openUnionHandle (return . Left) (runRescueT . handler) outerErrs

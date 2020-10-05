{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.Rescue.Constraint (MonadRescue) where

import           Control.Monad.Rescue.Class

type MonadRescue m = MonadRescueFrom m m

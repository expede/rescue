{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.Attempt.Constraint (CheckErrors) where

import           Control.Monad.Raise.Class
import           Data.WorldPeace

type CheckErrors m = Contains (Errors m) (Errors m)

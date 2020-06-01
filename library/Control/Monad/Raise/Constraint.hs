{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- |

module Control.Monad.Raise.Constraint
  ( Raises
  , RaisesAtLeast
  , RaisesOnly
  ) where

import           Control.Monad.Raise.Class

import           Data.WorldPeace
import           Data.WorldPeace.Subset.Class

-- FIXME docs
type Raises err m = Subset err (OpenUnion (Errors m))

-- FIXME docs
type RaisesAtLeast errs m = Subset (OpenUnion errs) (OpenUnion (Errors m))

-- FIXME docs
type RaisesOnly errs m = errs ~ Errors m
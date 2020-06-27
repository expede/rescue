{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Declare which exceptions may be raised/rescued in a given context

module Control.Monad.Raise.Constraint
  ( Raises
  , RaisesAtLeast
  , RaisesOnly
  ) where

import           Control.Monad.Raise.Class

import           Data.WorldPeace
import           Data.WorldPeace.Subset.Class

-- | Raises this exception, but potentially others
type Raises m err = Subset err (OpenUnion (Errors m))

-- | May raise errors, including the provided list
type RaisesAtLeast m errs = Subset (OpenUnion errs) (OpenUnion (Errors m))

-- | Restrict exceptions to exactly this list
type RaisesOnly m errs = errs ~ Errors m

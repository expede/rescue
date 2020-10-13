{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Declare which exceptions may be raised/rescued in a given context
module Control.Monad.Raise.Constraint
  ( -- * Permissive

    Raises
  , RaisesAtLeast

    -- * Precise

  , RaisesOnly
  , RaisesOne
  ) where

import           Control.Monad.Raise.Class

import           Data.WorldPeace
import           Data.WorldPeace.Subset.Class

-- | Raises this exception, but potentially others
type Raises m err = Subset err (ErrorCase m)

-- | May raise errors, including the provided list
type RaisesAtLeast m errs = Subset (OpenUnion errs) (ErrorCase m)

-- | Restrict exceptions to exactly this list
type RaisesOnly m errs = errs ~ Errors m

-- | Raises an exception, guaranteed to NOT be the outer union
type RaisesOne m err = IsMember err (Errors m)


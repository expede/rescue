{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Control.Monad.Rescue.Constraint
  ( Handles
  , MonadRescue
  ) where

import           Control.Monad.Raise.Class
import           Control.Monad.Rescue.Class

import           Data.WorldPeace

-- | Express the ability to 'handle' / eliminate an exception case
type Handles err n m
  =  ( ElemRemove       err (Errors n)
     , Contains (Remove err (Errors n)) (Errors m)
     )

-- | Rescue from the existing context only
--
-- Note: cannot 'handle' (elimininate) exceptions
type MonadRescue m = MonadRescueFrom m m

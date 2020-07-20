{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Data.WorldPeace.IsOpenUnion.Family (IsOpenUnion) where

import           Data.WorldPeace

-- | Type-level check that a type is an open union.
--   For use with the @Subset'@ typeclass.
type family (IsOpenUnion a) :: Bool where
  IsOpenUnion (OpenUnion a) = 'True
  IsOpenUnion a             = 'False

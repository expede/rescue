{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | FIXME docs

module Data.WorldPeace.IsOpenUnion.Family (IsOpenUnion) where

import           Data.WorldPeace

-- FIXME docs
type family (IsOpenUnion a) :: Bool where
  IsOpenUnion (OpenUnion a) = 'True
  IsOpenUnion a             = 'False

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME docs

module Data.WorldPeace.Subset.Class (Subset (..)) where

import           Data.Proxy
import           Data.WorldPeace
import           Data.WorldPeace.IsOpenUnion.Family
import           Data.WorldPeace.Subset.Class.Internal

-- FIXME docs

class Subset err errs where
  include :: err -> errs

instance (IsOpenUnion err ~ flag, Subset' flag err errs)
  => Subset err (OpenUnion errs) where
    include = include' (Proxy :: Proxy flag)

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.WorldPeace.Subset.Class (Subset (..)) where

import           Data.Proxy
import           Data.WorldPeace
import           Data.WorldPeace.IsOpenUnion.Family
import           Data.WorldPeace.Subset.Class.Internal

-- | State that some type @err@ is a subset of @errs@
--
--   In essence, this is used to avoid having to specially lift
--   bare values to an 'OpenUnion'. We can treat all types
--   and 'OpenUnion's the same.
class Subset err errs where
  include :: err -> errs

instance (IsOpenUnion err ~ flag, Subset' flag err errs) => Subset err (OpenUnion errs) where
  include = include' (Proxy @flag)
  {-# INLINE include #-}


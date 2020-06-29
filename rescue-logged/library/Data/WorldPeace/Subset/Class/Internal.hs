{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.WorldPeace.Subset.Class.Internal (Subset' (..)) where

import           Data.Kind
import           Data.Proxy
import           Data.WorldPeace

-- | Hack around overlapping instances to check subsets
--   of bare values or 'OpenUnion's automatically
class Subset' (flag :: Bool) (err :: Type) (errs :: [Type]) where
  include' :: Proxy flag -> err -> OpenUnion errs

instance Contains err errs => Subset' 'True (OpenUnion err) errs where
  include' _ = relaxOpenUnion

instance IsMember err errs => Subset' 'False err errs where
  include' _ = openUnionLift

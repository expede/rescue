{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | FIXME docs

module Data.WorldPeace.Subset.Class.Internal (Subset' (..)) where

import           Data.Kind
import           Data.Proxy
import           Data.WorldPeace

-- FIXME docs
-- NOTE 'flag :: Bool' is a hack around the overlapping instances problem
class Subset' (flag :: Bool) (err :: Type) (errs :: [Type]) where
  include' :: Proxy flag -> err -> OpenUnion errs

instance Contains err errs => Subset' 'True (OpenUnion err) errs where
  include' _ = relaxOpenUnion

instance IsMember err errs => Subset' 'False err errs where
  include' _ = openUnionLift

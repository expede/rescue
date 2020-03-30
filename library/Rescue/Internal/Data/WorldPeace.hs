{-# LANGUAGE FlexibleContexts #-}

-- |

module Rescue.Internal.Data.WorldPeace
  ( liftAs
  , relaxTo
  ) where

import           Data.Proxy
import           Data.WorldPeace

liftAs :: IsMember err errs => Proxy errs -> err -> OpenUnion errs
liftAs _proxy = openUnionLift

relaxTo :: Contains inner outer => Proxy outer -> OpenUnion inner -> OpenUnion outer
relaxTo _proxy = relaxOpenUnion

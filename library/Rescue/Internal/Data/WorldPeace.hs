{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |

module Rescue.Internal.Data.WorldPeace
  ( liftAs
  , relaxTo
  ) where

-- import           Data.Proxy
import           Data.WorldPeace

liftAs :: forall errs err . IsMember err errs => err -> OpenUnion errs
liftAs = openUnionLift

relaxTo :: forall outer inner . Contains inner outer => OpenUnion inner -> OpenUnion outer
relaxTo = relaxOpenUnion

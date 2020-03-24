{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module OpenUnion.Class (ToOpenUnion (..)) where

import           Data.WorldPeace

class ToOpenUnion elem variants where
  toOpenUnion :: elem -> OpenUnion variants

instance Contains inner outer => ToOpenUnion (OpenUnion inner) outer where
  toOpenUnion = relaxOpenUnion

instance {-# OVERLAPPING #-} IsMember elem variants => ToOpenUnion elem variants where
  toOpenUnion = openUnionLift

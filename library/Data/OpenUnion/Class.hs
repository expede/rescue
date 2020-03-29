{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.OpenUnion.Class (ToOpenUnion (..)) where

import           Data.Functor.Identity
import           Data.WorldPeace

class ToOpenUnion elem variants where
  toOpenUnion :: elem -> OpenUnion variants
  fromSingleton :: OpenUnion '[elem] -> OpenUnion variants

instance Contains inner outer => ToOpenUnion (OpenUnion inner) outer where
  toOpenUnion   = relaxOpenUnion
  fromSingleton (This (Identity target)) = relaxOpenUnion target

instance IsMember elem variants => ToOpenUnion elem variants where
  toOpenUnion   = openUnionLift
  fromSingleton = relaxOpenUnion

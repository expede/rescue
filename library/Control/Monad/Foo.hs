{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE TypeFamilies #-}

-- |

module Control.Monad.Foo (ToOpenUnion (..)) where

import Data.Kind
import Data.WorldPeace

class ToOpenUnion (union :: [Type]) a where
  consistent :: a -> OpenUnion union

instance IsMember elem set => ToOpenUnion set elem where
  consistent inner = openUnionLift inner

instance Contains inner outer => ToOpenUnion outer (OpenUnion inner) where
  consistent inner = relaxOpenUnion inner

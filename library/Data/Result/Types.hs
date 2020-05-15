{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PatternSynonyms #-}

-- | FIXME docs

module Data.Result.Types
  ( Result
  , pattern Ok
  , pattern Err
  ) where

import           Data.Kind
import           Data.WorldPeace

-- FIXME docs
type Result (errs :: [Type]) = Either (OpenUnion errs)

pattern Ok :: val -> Either err val
pattern Ok val = Right val

pattern Err :: a -> Either a b
pattern Err err = Left err

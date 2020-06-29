{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Result.Types
  ( Result
  , pattern Ok
  , pattern Err
  ) where

import           Data.Kind
import           Data.WorldPeace

-- | 'Result' is a synonym for 'Either (OpenUnion errs) a'
type Result (errs :: [Type]) = Either (OpenUnion errs)

pattern Ok :: val -> Either (OpenUnion err) val
pattern Ok val = Right val

pattern Err :: OpenUnion errs -> Either (OpenUnion errs) b
pattern Err errs = Left errs

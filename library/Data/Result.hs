{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | 'Result' is a

module Data.Result
  ( fromEither
  -- * Reexports
  , module Data.Result.Types
  ) where

import           Data.Result.Types
import           Data.WorldPeace

import           Control.Monad.Raise

-- | Upgrade from an 'Either' to a 'Result'
fromEither :: Subset err (OpenUnion errs) => Either err val -> Result errs val
fromEither (Right val) = Ok val
fromEither (Left  err) = Err (include err)

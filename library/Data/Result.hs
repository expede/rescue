{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | FIXME docs

module Data.Result
  ( fromEither
  -- * Reexports
  , module Data.Result.Types
  ) where

import Data.Result.Types
import Data.WorldPeace

import Control.Monad.Raise.Class

-- FIXME docs
fromEither :: Convert err (OpenUnion errs) => Either err val -> Result errs val
fromEither (Right val) = Ok val
fromEither (Left  err) = Err (convert err)

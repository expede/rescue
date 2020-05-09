{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |

module Control.Monad.Raise.Unsafe () where

import           Control.Monad.Raise.Class

import           GHC.Base
import           GHC.ST
import           GHC.IO

instance MonadRaise errs (ST s) where
  raise = GHC.IO.unsafeIOToST . raise

instance MonadRaise errs IO where
  raise = IO . raiseIO#

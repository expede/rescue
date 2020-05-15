{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |

module Control.Monad.Raise.Unsafe () where

import           Control.Monad.Raise.Class


import Control.Exception

import           GHC.Base
import           GHC.ST
import           GHC.IO

instance MonadRaise (ST s) where
  type Errors (ST s) = '[IOException]
  raise = GHC.IO.unsafeIOToST . raise

instance MonadRaise IO where
  type Errors IO = '[IOException]
  raise = IO . raiseIO#

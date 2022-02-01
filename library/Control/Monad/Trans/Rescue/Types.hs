-- | The 'RescueT' transformer
module Control.Monad.Trans.Rescue.Types
  ( RescueT
  , Rescue
  ) where

import           Control.Monad.Trans.Except

import           Data.WorldPeace

-- | Add type-directed error handling abilities to a 'Monad'
type RescueT errs m a = ExceptT (OpenUnion errs) m a

-- | A specialized version of 'RescueT' to be used without a transfromer stack
type Rescue errs a = Except (OpenUnion errs) a

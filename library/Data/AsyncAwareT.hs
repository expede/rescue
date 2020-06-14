-- |

module Data.AsyncAwareT where

import Data.AsyncAwareT.Types

-- FIXME MOVE TO UPPER MODUEL
type AsyncAwareIO a = AsyncAwareT IO a

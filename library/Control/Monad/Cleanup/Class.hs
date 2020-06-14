{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | FIXME add docs

module Control.Monad.Cleanup.Class (MonadCleanup (..)) where

import           Control.Monad.Catch  as Catch
import           Control.Monad.Rescue

import           Data.WorldPeace

-- FIXME move somewhere better and rename
-- type OpenErrors m = OpenUnion (Errors m)

-- | Safely work with resources when an asynchronous exception may be thrown
class (Raises SomeException m, MonadRescue m) => MonadCleanup m where
  cleanup
    :: m resource                                   -- ^ Acquire some resource
    -> (resource -> OpenUnion (Errors m) -> m _ig1) -- ^ Cleanup and re-raise
    -> (resource ->                         m _ig2) -- ^ Cleanup normally
    -> (resource ->                         m a)    -- ^ Inner action to perform with the resource
    -> m a

type AsyncAwareIO a = AsyncAwareT IO a

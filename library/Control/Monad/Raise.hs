{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Monadic raise semantics & helpers

module Control.Monad.Raise
  ( ensure
  -- * Reexports
  , module Control.Monad.Raise.Class
  , module Data.WorldPeace.Subset.Class
  ) where

import           Data.WorldPeace
import           Data.WorldPeace.Subset.Class

import           Control.Monad.Raise.Class

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
-- >>>
-- >>> import Data.Proxy
-- >>> import Data.WorldPeace
-- >>>
-- >>> data FooErr  = FooErr  deriving Show
-- >>> data BarErr  = BarErr  deriving Show
-- >>> data QuuxErr = QuuxErr deriving Show

-- | Lift a pure error (@Either@) into a @MonadRaise@ context
-- i.e. Turn @Left@s into @raise@s.
--
-- ==== __Examples__
--
-- FIXME more examples for more cases of the automated behaviour!!
--
-- >>> :{
--   mayFail :: Int -> Either FooErr) Int
--   mayFail n =
--     if n > 50
--       then raise FooErr
--       else pure n
-- :}
--
-- >>> :{
--   foo :: MonadRaise MyErrs m => m Int
--   foo = do
--     first  <- ensure $ mayFail 100
--     second <- ensure $ mayFail first
--     return $ second * 10
-- :}
--
-- >>> foo :: Maybe Int
-- Nothing
ensure
  :: ( MonadRaise m
     , Errors m ~ outerErrs
     , Subset inner (OpenUnion outerErrs)
     )
  => Either inner a
  -> m a
ensure = \case
  Left  err -> raise err
  Right val -> pure val

-- FIXME add stylish haskell because why not

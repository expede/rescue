{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE DataKinds #-}

-- | Monadic raise semantics

module Control.Monad.Raise
  ( module Control.Monad.Raise.Class
  , raise
  , raiseInto
  -- , fromEither
  ) where

import           Control.Monad.Raise.Class
import           Data.WorldPeace

raise :: forall as errs m a .
  ( Contains as errs
  , MonadRaise errs m
  )
  => OpenUnion as
  -> m a
raise err = raise' outerErr
  where
    outerErr :: OpenUnion errs
    outerErr = relaxOpenUnion err

raiseInto :: forall err errs m a .
  ( IsMember err errs
  , MonadRaise errs m
  )
  => err
  -> m a
raiseInto err = raise' outerErr
  where
    outerErr :: OpenUnion errs
    outerErr = openUnionLift err

-- fromEither ::
--   ( IsMember err errs
--   , MonadRaise errs m
--   )
--   => Either err a
--   -> m a
-- fromEither (Right val) = pure val
-- fromEither (Left  err) = raiseInto err
--   where


-- fromEitherM :: forall inner outer m a .
--   ( Contains inner outer
--   , MonadRaise inner       m
--   , MonadRaise       outer m
--   )
--   => m (Either (OpenUnion inner) a)
--   -> m a
-- fromEitherM action = fromEither =<< action

data FakeErr = FakeErr
data OtherErr = OtherErr

type Errs = '[FakeErr, OtherErr]

foo :: MonadRaise Errs m => Either FakeErr a -> m a
foo = \case
  Left FakeErr -> raise' (openUnionLift FakeErr :: OpenUnion Errs)
  Right ok -> pure ok

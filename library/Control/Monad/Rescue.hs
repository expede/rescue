{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Rescue semantics & helpers
--
-- Essentially a type-directed version of 'Control.Monad.Catch'.
--
-- This is the opposite of 'Control.Monad.Raise', which embeds en error.
-- 'Rescue' takes a potential error out of the surrounding context
-- and either handles or exposes it.
module Control.Monad.Rescue
  ( attemptM
  , handle
  , handleBase
  , handleM
  , handleEach
  , handleEachM
  , handleAll

  -- * Guaranteed runs

  , reattempt
  , onRaise
  , lastly

  -- * Error access

  , mapError
  , replaceError
  , asNotFound

  -- * Reexports

  , module Control.Monad.Raise
  , module Control.Monad.Rescue.Class
  ) where

import           Data.Exception.Types
import           Data.Result.Types
import           Numeric.Natural

import           Control.Monad.Base
import           Data.Bifunctor             as Bifunctor
import           Data.WorldPeace

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
--
-- >>> import Control.Monad.Trans.Rescue
-- >>> import Data.Proxy
-- >>> import Data.WorldPeace as OpenUnion
--
-- >>> data FooErr  = FooErr  deriving Show
-- >>> data BarErr  = BarErr  deriving Show
-- >>> data QuuxErr = QuuxErr deriving Show

-- | Simpler helper to eliminate the bind operator from an attempt flow
--
-- >>> type MyErrs = '[FooErr, BarErr]
-- >>> boom = Left FooErr
--
-- >>> :{
-- handled :: Rescue MyErrs String
-- handled =
--   attempt boom >>= \case
--     Left  err -> handle err
--     Right val -> return val
-- :}
--
-- >>> :{
--  attemptM boom \case
--   Left  err -> handle err
--   Right val -> return val
-- :}
attemptM :: MonadRescue m => m a -> (Either (ErrorCase m) a -> m b) -> m b
attemptM action handler = attempt action >>= handler

handle
  :: ( Bifunctor m
     , ElemRemove err errs
     )
  => (err -> OpenUnion (Remove err errs))
  -> m (OpenUnion             errs)  a
  -> m (OpenUnion (Remove err errs)) a
handle handler action = Bifunctor.first (openUnionHandle id handler) action

-- | The more generic (MonadBase-ified) version of handle
handleBase
  :: ( MonadRescue wide
     , MonadBase   wide narrow
     , MonadRaise       narrow
     , CheckErrors      narrow
     , Errors narrow ~ Remove err (Errors wide)
     , ElemRemove err (Errors wide)
     )
  => (err -> narrow a)
  -> wide   a
  -> narrow a
handleBase handler action =
  liftBase (attempt action) >>= \case
    Left err    -> openUnionHandle raise handler err
    Right value -> return value

handleM
  :: ( MonadBase (m (OpenUnion errs)) (m (OpenUnion (Remove err errs)))
     --
     , MonadRescue (m (OpenUnion errs))
     , MonadRaise  (m (OpenUnion (Remove err errs)))
     --
     ,  errs ~ Errors (m (OpenUnion errs))
     , ElemRemove err errs
     , Contains (Remove err errs) (Errors (m (OpenUnion (Remove err errs))))
     )
  => (err -> m (OpenUnion (Remove err errs)) a)
  -> m (OpenUnion             errs)  a
  -> m (OpenUnion (Remove err errs)) a
handleM handler action =
  liftBase (attempt action) >>= \case
    Right val ->
      return val

    Left errs ->
      case openUnionRemove errs of
        Left  remainingErrs -> raise remainingErrs
        Right matchedErr    -> handler matchedErr

handleEach
  :: ( Bifunctor m
     , ToOpenProduct handlerTuple (ReturnX (OpenUnion narrowErrs) errs)
     )
  => handlerTuple
  -> m (OpenUnion errs)       a
  -> m (OpenUnion narrowErrs) a
handleEach handleCases action = Bifunctor.first (catchesOpenUnion handleCases) action

handleEachM
  :: ( errs ~ Errors (m (OpenUnion errs))
     , MonadRescue   (m (OpenUnion errs))
     , MonadBase     (m (OpenUnion errs))  (m (OpenUnion narrowErrs))
     , ToOpenProduct handlerTuple (ReturnX (m (OpenUnion narrowErrs) a) errs)
     )
  => handlerTuple
  -> m (OpenUnion errs) a
  -> m (OpenUnion narrowErrs) a
handleEachM handleCases action =
  liftBase (attempt action) >>= \case
    Left errs -> catchesOpenUnion handleCases errs
    Right val -> return val

handleAll
  :: ( MonadRescue   (m (OpenUnion errs))
     , MonadBase     (m (OpenUnion errs)) (m ())
     , errs ~ Errors (m (OpenUnion errs))
     )
  => (OpenUnion errs -> m () a)
  -> m (OpenUnion errs) a
  -> m () a
handleAll handler action =
  liftBase (attempt action) >>= \case
    Left errs -> handler errs
    Right val -> return val

onRaise
  :: ( MonadRescue m
     , RaisesOnly  m errs
     )
  => (OpenUnion errs -> m ())
  -> m a
  -> m (Result errs a)
onRaise errHandler action =
  attempt action >>= \case
    Left err -> do
      errHandler err
      return $ Err err

    Right val ->
      return $ Ok val

-- | 'retry' without asynchoronous exception cleanup.
--   Useful when not dealing with external resources that may
--   be dangerous to close suddenly.
reattempt :: MonadRescue m => Natural -> m a -> m a
reattempt 0     action = action
reattempt times action =
  attempt action >>= \case
    Left  _   -> reattempt (times - 1) action
    Right val -> return val

-- | Run an additional step, and throw away the result.
--   Return the result of the action passed.
lastly
  :: ( CheckErrors m
     , MonadRescue m
     )
  => m a
  -> m b
  -> m a
lastly action finalizer = do
  errOrOk <- attempt action
  _       <- finalizer
  ensure errOrOk

-- AKA reinterpret
mapError
  :: ( MonadRescue m
     , MonadBase   m n
     , MonadRaise    n
     , CheckErrors   n
     )
  => (ErrorCase m -> ErrorCase n)
  -> m a
  -> n a
mapError mapper action =
  liftBase (attempt action) >>= \case
    Left  errCaseN -> raise $ mapper errCaseN
    Right value    -> return value

replaceError
  :: ( MonadRescue m
     , MonadBase   m n
     , MonadRaise    n
     , n `Raises` err
     )
  => err
  -> m a
  -> n a
replaceError err action =
  liftBase (attempt action) >>= \case
    Left  _     -> raise err
    Right value -> return value

asNotFound
  :: forall n m a .
    ( MonadRescue m
    , MonadBase   m n
    , MonadRaise    n
    , n `Raises` NotFound a
    )
  => m a
  -> n a
asNotFound = replaceError (NotFound @a)

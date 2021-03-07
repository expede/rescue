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

  -- * Recover from exceptions

  , rescue
  , rescueT
  , rescueM
  , rescueBase

  , rescueEach
  , rescueEachM
  , rescueEachT

  , rescueAll

  -- * Guaranteed runs

  , reattempt
  , report
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
import           Numeric.Natural

import           Control.Monad.Base
import           Data.Bifunctor             as Bifunctor
import           Data.WorldPeace

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class
import           Control.Monad.Trans.Error

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
-- >>> :set -XLambdaCase
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
--
-- >>> :{
-- boom :: Rescue MyErrs String
-- boom = raise FooErr
-- :}
--
-- >>> :{
-- attempt boom >>= \case
--   Left  err -> return ("err: " ++ show err)
--   Right val -> return val
-- :}
-- RescueT (Identity (Right "err: Identity FooErr"))
--
-- >>> :{
-- attemptM boom $ \case
--   Left  err -> return ("err: " ++ show err)
--   Right val -> return val
-- :}
-- RescueT (Identity (Right "err: Identity FooErr"))
attemptM :: MonadRescue m => m a -> (Either (ErrorCase m) a -> m b) -> m b
attemptM action handler = attempt action >>= handler

rescue
  :: ( Bifunctor m
     , ElemRemove err errs
     )
  => (err -> OpenUnion (Remove err errs))
  -> m (OpenUnion             errs)  a
  -> m (OpenUnion (Remove err errs)) a
rescue handler = Bifunctor.first (openUnionHandle id handler)

-- | Handle and eliminate a single error
rescueT ::
  ( MonadTransError t errs m
  , MonadRaise  (t (Remove err errs) m)
  , CheckErrors (t (Remove err errs) m)
  , ElemRemove err (Errors (t errs m))
  , Remove     err (Errors (t errs m)) ~ Errors (t (Remove err errs) m)
  )
  => (err -> (t (Remove err errs)) m a)
  -> t             errs  m a
  -> t (Remove err errs) m a
rescueT handler = onRaise (openUnionHandle raise handler)

-- | The more generic (MonadBase-ified) version of handle
rescueBase
  :: ( MonadRescue wide
     , MonadBase   wide narrow
     , MonadRaise       narrow
     , CheckErrors      narrow
     , Remove     err (Errors wide) ~ Errors narrow
     , ElemRemove err (Errors wide)
     )
  => (err -> narrow a)
  -> wide   a
  -> narrow a
rescueBase handler action =
  liftBase (attempt action) >>= \case
    Left err    -> openUnionHandle raise handler err
    Right value -> return value

rescueM
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
rescueM handler action =
  liftBase (attempt action) >>= \case
    Right val ->
      return val

    Left errs ->
      case openUnionRemove errs of
        Left  remainingErrs -> raise remainingErrs
        Right matchedErr    -> handler matchedErr

rescueEach
  :: ( Bifunctor m
     , ToOpenProduct handlerTuple (ReturnX (OpenUnion targetErrs) errs)
     )
  => handlerTuple
  -> m (OpenUnion errs)       a
  -> m (OpenUnion targetErrs) a
rescueEach handleCases = Bifunctor.first (catchesOpenUnion handleCases)

rescueEachM
  :: ( sourceErrs ~ Errors (m (OpenUnion sourceErrs))
     , MonadRescue         (m (OpenUnion sourceErrs))
     , MonadBase           (m (OpenUnion sourceErrs)) (m (OpenUnion targetErrs))
     , ToOpenProduct handlerTuple            (ReturnX (m (OpenUnion targetErrs) a) sourceErrs)
     )
  => handlerTuple
  -> m (OpenUnion sourceErrs) a
  -> m (OpenUnion targetErrs) a
rescueEachM handleCases action =
  liftBase (attempt action) >>= \case
    Left errs -> catchesOpenUnion handleCases errs
    Right val -> return val

rescueEachT
  :: ( sourceErrs ~ Errors (t sourceErrs m)
     , MonadTransError      t sourceErrs m
     , ToOpenProduct handlerTuple (ReturnX (t targetErrs m a) sourceErrs)
     )
  => handlerTuple
  -> t sourceErrs m a
  -> t targetErrs m a
rescueEachT handleCases = onRaise (catchesOpenUnion handleCases)

rescueAll
  :: ( MonadRescue   (m (OpenUnion errs))
     , MonadBase     (m (OpenUnion errs)) (m ())
     , errs ~ Errors (m (OpenUnion errs))
     )
  => (OpenUnion errs -> m () a)
  -> m (OpenUnion errs) a
  -> m () a
rescueAll handler action =
  liftBase (attempt action) >>= \case
    Left errs -> handler errs
    Right val -> return val

report
  :: ( MonadRescue m
     , RaisesOnly  m errs
     , CheckErrors m
     )
  => (ErrorCase m -> m ())
  -> m a
  -> m a
report withErr action =
  attempt action >>= \case
    Left err -> do
      withErr err
      raise err

    Right val ->
      return val

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

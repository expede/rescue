{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | 

module Control.Monad.Reraise where

import Control.Monad.Raise
 
import           Data.Proxy
import           Data.WorldPeace



 

import Control.Monad.Foo

import           Data.Proxy
import           Data.WorldPeace

import           Control.Monad.Catch.Pure
import           Control.Monad.Cont

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

import qualified Control.Monad.RWS.Lazy   as Lazy
import qualified Control.Monad.RWS.Strict as Strict

import qualified Control.Monad.State.Lazy   as Lazy
import qualified Control.Monad.State.Strict as Strict

import qualified Control.Monad.Writer.Lazy   as Lazy
import qualified Control.Monad.Writer.Strict as Strict


-- FIXME So, I gues sthis is just a MonadRaise function!
-- FIXME needs a better name / ensure
-- reraise :: forall err(ToOpenUnion errs outer, MonadRaise (OpenUnion outer) m) => Either errs a -> m a
-- reraise = \case
--   Right val -> pure val
--   Left  err -> raise @(OpenUnion outer) $ consistent err

-- reraiseM :: MonadRaise innerErrs m => m (Either innerErrs a) -> m a
-- reraiseM action = reraise =<< action

-- -- NOTE TO SELF: this shoudl actaully just be a natual transformation, i.e. n a -> m a, becasue n and m have all the error carrying info
-- class MonadRaise outerErrs m => MonadReraise innerErrs outerErrs m where
--   reraise :: (Either (OpenUnion innerErrs) a) -> a -> m a
--   -- ^ TODO: rename function to `relax` or `relaxErr`? `recontextualize`?
--   -- ^ TODO: rename class to `MonadCleanup`?

class MonadReraise n m where
  recontextualize :: n a -> m a
 
instance MonadReraise m m where
  recontextualize action = action

-- instance MonadReraise errs errs m where
--   reraise action = action
 
-- reraise :: forall outerErr innerErr n m a .
--   ( ToOpenUnion (OpenUnion innerErr) outerErr
--   , MonadRaise (OpenUnion outerErr) m
--   , MonadRescue innerErr n
--   )
--   => n a
--   -> m a
-- reraise = try action >>= \case
--   Left  innerErr -> raise @(OpenUnion outerErr) $ consistent innerErr
--   Right value    -> pure value

-- recontextualize :: MonadReraise outer m n => Proxy outer -> m a -> n a
-- recontextualize = reraise

-- rectx :: MonadReraise outer m n => Proxy outer -> m a -> n a
-- rectx = recontextualize

-- instance (ToOpenUnion inner outer, MonadRaise (OpenUnion outer) m, MonadRescue inner n) => MonadReraise n m where
--   reraise action = try action >>= \case

instance MonadReraise Maybe [] where
  recontextualize Nothing  = []
  recontextualize (Just x) = [x]

instance MonadReraise (Either errs) Maybe where
  recontextualize (Left  _)   = Nothing
  recontextualize (Right val) = Just val

instance MonadReraise (Either errs) [] where
  recontextualize (Left  _)   = []
  recontextualize (Right val) = [val]

instance ToOpenUnion inner outer => MonadReraise (Either inner) (Either (OpenUnion outer)) where
  recontextualize (Left err)  = raise err
  recontextualize (Right val) = pure val

-- Transformers

-- instance (Monad m, MonadReraise n m) => MonadReraise n (MaybeT m) where
--   recontextualize = lift . recontextualize

instance (Monad m, MonadTrans t, MonadReraise n m) => MonadReraise n (t m) where
  recontextualize = lift . recontextualize

-- reraise action = try action

-- instance MonadReraise n m => MonadReraise n (IdentityT m) where
--   recontextualize = lift . recontextualize

-- instance MonadReraise n m => MonadReraise n (ExceptT errs m) where
--   recontextualize = lift . recontextualize

-- instance MonadReraise n m => MonadReraise errs (ReaderT cfg m) where
--   recontextualize = lift . recontextualize

-- instance MonadReraise n m => MonadReraise errs (CatchT m) where
--   recontextualize = lift . recontextualize

-- instance MonadReraise n m => MonadReraise errs (ContT r m) where
--   recontextualize = lift . recontextualize

-- instance MonadReraise n m => MonadReraise errs (Lazy.StateT s m) where
--   recontextualize = lift . recontextualize

-- instance MonadReraise n m => MonadReraise errs (Strict.StateT s m) where
--   recontextualize = lift . recontextualize

-- instance (Monoid w, MonadReraise errs m) => MonadReraise errs (Lazy.WriterT w m) where
--   recontextualize = lift . recontextualize

-- instance (Monoid w, MonadReraise errs m) => MonadReraise errs (Strict.WriterT w m) where
--   recontextualize = lift . recontextualize

-- instance (MonadReraise errs m, Monoid w) => MonadReraise errs (Lazy.RWST r w s m) where
--   recontextualize = lift . recontextualize

-- instance (MonadReraise errs m, Monoid w) => MonadReraise errs (Strict.RWST r w s m) where
--   recontextualize = lift . recontextualize

-- data NotFound subject = NotFound
-- instance forall subject errs . IsMember (NotFound subject) errs => MonadReraise Maybe (Either (OpenUnion errs)) where
--   recontextualize Nothing    = raise (NotFound @subject)
--   recontextualize (Just val) = pure val

-- -- instance IsMember err errs =>
-- --   MonadReraise errs (Either err) (Either (OpenUnion errs)) where
-- --     reraise _ = \case
-- --       Left  err -> raise @(OpenUnion errs) err
-- --       Right val -> return val

-- instance (MonadRescue innerErrs (Either innerErrs), ToOpenUnion innerErrs outerErrs) =>
--   MonadReraise outerErrs (Either innerErrs) (Either (OpenUnion outerErrs)) where
--     reraise _pxyOuterErrs = \case
--       Left  err -> raise err
--       Right val -> return val

-- reraise :: forall outer inner n m a . (ToOpenUnion (OpenUnion inner) outer, MonadRescue inner n, MonadRaise (OpenUnion outer) m) => n a -> m a
-- reraise action = do
--   let innerResult = try @inner action
--   case innerResult of
--     Left err -> raise $ consistent err


-- cleanup :: forall outer n m resource output ignored1 ignored2 .
--   ( MonadRescue outer m
--   -- , MonadReraise outer n m
--   )
--   => Proxy outer
--   -> n resource                                  -- ^ Acquire resource
--   -> (resource -> OpenUnion outer -> m ignored1) -- ^ Cleanup exception case; The exception will be reraised
--   -> (resource -> output          -> m ignored2) -- ^ Cleanup happy path
--   -> (resource -> m output)                      -- ^ Inner action
--   -> m output
-- cleanup pxyO acquire onErr onOk action = do
--   resource <- reraise acquire
--   try @outer (action resource) >>= \case
--     Left err -> do
--       _ <- onErr resource err
--       raise err

--     Right output -> do
--       _ <- onOk resource output
--       return output

-- cleanup' :: forall outer n m resource output ignored1 ignored2 .
--   ( MonadRescue outer m
--   -- , MonadReraise outer n m
--   )
--   => n resource                                  -- ^ Acquire resource
--   -> (resource -> OpenUnion outer -> m ignored1) -- ^ Cleanup exception case; The exception will be reraised
--   -> (resource -> output          -> m ignored2) -- ^ Cleanup happy path
--   -> (resource -> m output)                      -- ^ Inner action
--   -> m output
-- cleanup' = cleanup (Proxy @outer)

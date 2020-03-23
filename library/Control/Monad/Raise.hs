{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE UndecidableInstances       #-} -- FIXME!

-- | Monadic raise semantics

module Control.Monad.Raise
  ( MonadRaise  (..)
  , MonadRescue (..)
  , EnsureT (..) -- ... maybe
  , rescue
  , raise
  , reraise
  , handle
  , ensure
  ) where

import           Data.WorldPeace
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

class ToOpenUnion elem variants where
  toOpenUnion :: elem -> OpenUnion variants

instance {-# OVERLAPPABLE #-} Contains inner outer => ToOpenUnion (OpenUnion inner) outer where
  toOpenUnion = relaxOpenUnion

instance {-# OVERLAPPING #-} IsMember elem variants => ToOpenUnion elem variants where
  toOpenUnion x = openUnionLift x

------------------------------------------------

class Monad m => MonadRaise errs m where
  raise' :: OpenUnion errs -> m a

instance MonadRaise errs [] where
  raise' _ = []

instance MonadRaise errs Maybe where
  raise' _ = Nothing

instance MonadRaise errs (Either (OpenUnion errs)) where
  raise' = Left

instance Monad m => MonadRaise errs (ExceptT (OpenUnion errs) m) where
  raise' = ExceptT . pure . Left

raise :: forall m err errs a .
  ( ToOpenUnion err errs
  , MonadRaise      errs m
  )
  => err
  -> m a
raise err = raise' (toOpenUnion err :: OpenUnion errs)

-------------------------------------------------------------

class MonadRaise errs m => MonadRescue errs m where
  expose :: m a -> m (Either (OpenUnion errs) a)

instance MonadRescue errs (Either (OpenUnion errs)) where
  expose = Right
  {-# INLINE expose #-}

instance Monad m => MonadRescue errs (ExceptT (OpenUnion errs) m) where
  expose (ExceptT action) = ExceptT (expose <$> action)

rescue :: MonadRescue errs m => m a -> (Either (OpenUnion errs) a -> m b) -> m b
rescue action handler = expose action >>= handler

reraise :: forall innerErrs outerErrs m .
  ( Contains    innerErrs  outerErrs
  , MonadRescue innerErrs            m
  , MonadRaise             outerErrs m
  )
  => m ()
  -> m ()
reraise action = rescue action handler
  where
    handler :: Either (OpenUnion innerErrs) () -> m ()
    handler = \case
      Left  err -> raise' (relaxOpenUnion err :: OpenUnion outerErrs)
      Right ()  -> return ()

handle :: MonadRescue errs m => m a -> (OpenUnion errs -> m a) -> m a
handle action handler = expose action >>= either handler pure

------------------------------

-- | Maybe this is just what raise is? But also may have fewer instanes...
class MonadRaise errs m => MonadEnsure errs m where
  ensure' :: Either (OpenUnion errs) a -> m a

instance MonadEnsure errs (Either (OpenUnion errs)) where
  ensure' = id

instance Monad m => MonadEnsure errs (ExceptT (OpenUnion errs) m) where
  ensure' = ExceptT . pure

ensure :: forall err errs m a .
  ( ToOpenUnion err errs
  , MonadEnsure errs m
  )
  => Either err a
  -> m a
ensure = \case
  Left err  -> ensure' $ Left (toOpenUnion err :: OpenUnion errs)
  Right val -> pure val

-------------------------------------

newtype EnsureT errs m a = EnsureT { runEnsureT :: ExceptT (OpenUnion errs) m a }
  deriving (Functor, Applicative, Monad)

instance MonadTrans (EnsureT errs) where
  lift = EnsureT . lift
  {-# INLINE lift #-}

instance Monad m => MonadRaise errs (EnsureT errs m) where
  raise' = EnsureT . raise'

instance Monad m => MonadRescue errs (EnsureT errs m) where
  expose = EnsureT . expose . runEnsureT

instance Monad m => MonadEnsure errs (EnsureT errs m) where
  ensure' = EnsureT . ExceptT . pure

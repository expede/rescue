{-# LANGUAGE MultiParamTypeClasses      #-}

-- | Equivalent to MonadMask / MonadBracket pattern

module Control.Monad.Ensure () where

-- import Data.WorldPeace

-- import Control.Monad.Rescue

-- import Control.Monad.Catch (ExitCase (..))

-- -- data AbortWith a =

-- class MonadRescue errs m => MonadEnsure errs m where
--   ensure ::
--        m a -- ^ acquire some resource
--     -> (a              -> m c) -- ^ cleanup when no exception was raised
--     -> (OpenUnion errs -> m c) -- ^ cleanup when an exception was raised
--     -> (a              -> m b) -- ^ inner action to perform with the resource
--     -> m b

-- -- BRACKET
-- ensure ::
--   MonadRescue err m
--   => m a
--   -> (a -> m cleanup)
--   -> (a -> m inner)
--   -> m inner
-- ensure acquire release innerAction = do
--   acquire >>=

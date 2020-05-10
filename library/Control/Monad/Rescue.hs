{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Rescue semantics & helpers
--
-- Essentially a type-directed version of 'Control.Monad.Catch'.
--
-- This is the opposite of 'Control.Monad.Raise', which embeds en error.
-- 'Rescue' takes a potential error out of the surrounding context
-- and either handles or exposes it.

module Control.Monad.Rescue
  (
  -- * Reexports

    module Control.Monad.Raise
  , module Control.Monad.Rescue.Class
 
  -- * 'try' Helpers
 
  -- , try'
 
  , rescue
  -- , rescue'
  -- , rescueWith

  -- , rescueM
  -- , rescueM'
 
  -- -- , reraiseTo
  -- , handle
  -- , handleOne

  , cleanup
 
  , finally
  -- , finally'
  ) where

import           Control.Monad.Raise
import           Control.Monad.Rescue.Class
 
import           Data.Proxy
import           Data.WorldPeace



import Control.Monad.Foo

-- import Rescue.Internal.Data.WorldPeace

-- -- $setup
-- --
-- -- >>> :set -XDataKinds
-- -- >>> :set -XFlexibleContexts
-- -- >>> :set -XTypeApplications
-- --
-- -- >>> import Control.Monad.Trans.Rescue
-- -- >>> import Data.Proxy
-- -- >>> import Data.WorldPeace as OpenUnion
-- --
-- -- >>> data FooErr  = FooErr  deriving Show
-- -- >>> data BarErr  = BarErr  deriving Show
-- -- >>> data QuuxErr = QuuxErr deriving Show

-- -- | A version of 'try' that infers the 'Proxy' from context
-- --
-- -- >>> type MyErrs = '[FooErr, BarErr]
-- --
-- -- >>> :{
-- --  goesBoom :: Int -> Rescue MyErrs Int
-- --  goesBoom x =
-- --    if x > 50
-- --      then return x
-- --      else raiseAs @MyErrs FooErr
-- -- :}
-- --
-- -- >>> try' $ goesBoom 42 :: Rescue MyErrs ((Either (OpenUnion MyErrs) Int))
-- -- RescueT (Identity (Right (Left (Identity FooErr))))
-- try' :: forall m a errs . MonadRescue errs m => m a -> m (Either (OpenUnion errs) a)
-- try' = try @errs

-- -- | FIXME add one-liner
-- --
-- -- >>> type InnerErrs = '[FooErr, BarErr]
-- -- >>> type OuterErrs = '[FooErr, BarErr, QuuxErr]
-- --
-- -- >>> :{
-- -- innerBoom :: Int -> Rescue InnerErrs Int
-- -- innerBoom x =
-- --   if x > 50
-- --     then return x
-- --     else raiseAs (Proxy @InnerErrs) FooErr
-- -- :}
-- --
-- -- >>> :{
-- -- outerBoom :: [x] -> Rescue OuterErrs [x]
-- -- outerBoom [] = return []
-- -- outerBoom list@(x:xs) = do
-- --   let attempt = (innerBoom (length list) :: Rescue InnerErrs Int)
-- --   okCount <- reraiseTo (Proxy @OuterErrs) (Proxy @InnerErrs) attempt
-- --   return . take okCount $ repeat x
-- -- :}
-- --
-- -- >>> outerBoom [1,2,3] :: Rescue OuterErrs [Int]
-- -- RescueT (Identity (Right (Left (Identity FooErr))))
-- reraiseTo :: forall inner outer m n a .
--   ( Contains    inner outer
--   , MonadRescue inner       n
--   , MonadRaise        outer m
--   , Traversable n
--   , n ~ m
--   )
--   => Proxy outer
--   -> Proxy inner
--   -> n a
--   -> m a
-- reraiseTo pxyO pxyI action =
--   case sequence (try pxyI action) :: Either (OpenUnion inner) (m a) of
--     Left  err  -> raise @outer $ (relaxTo pxyO err :: OpenUnion outer) -- raiseTo pxyO err -- has outer errs
--     Right mVal -> (mVal :: m a) -- has inner errors

-- | FIXME add one-liner
--
-- >>> type MyErrs = '[FooErr, BarErr]
-- >>> myErrs = Proxy @MyErrs
--
-- >>> :{
-- goesBoom :: Int -> Rescue MyErrs String
-- goesBoom x =
--   if x > 50
--     then return (show x)
--     else raiseAs @MyErrs FooErr
-- :}
--
-- >>> handler = catchesOpenUnion (\foo -> "Foo: " <> show foo, \bar -> "Bar:" <> show bar)
-- >>> rescue myErrs (goesBoom 42) (pure . handler)
-- RescueT (Identity (Right "Foo: FooErr"))
rescue :: forall errs err m a .
  MonadRescue errs m
  => m a
  -> (OpenUnion errs -> m a)
  -> m a
rescue action handler = either handler pure =<< try action

-- -- | A version of @ensure@ that takes monadic actions
-- --
-- -- ==== __Examples__
-- --
-- -- >>> :{
-- --   mayFailM :: Monad m => Int -> m (Either (OpenUnion MyErrs) Int)
-- --   mayFailM n =
-- --     return $ if n > 50
-- --       then Left (openUnionLift FooErr)
-- --       else Right n
-- -- :}
-- --
-- -- >>> type BigErrs = '[FooErr, BarErr, QuuxErr]
-- --
-- -- >>> :{
-- --   foo :: MonadRaise BigErrs m => m Int
-- --   foo = do
-- --     first  <- ensureM @BigErrs $ mayFailM 100
-- --     second <- ensureM @BigErrs $ mayFailM first
-- --     return (second * 10)
-- -- :}
-- --
-- -- >>> foo :: Maybe Int
-- -- Nothing
-- ensureM :: forall outer inner m a errs .
--   ( ToOpenUnion inner outer
--   , MonadRaise (OpenUnion outer) m
--   , errs ~ OpenUnion outer
--   )
--   => m (Either inner a)
--   -> m a
-- ensureM action = ensure =<< try action

-- reraise :: forall outerErr innerErr n m a .
--   ( ToOpenUnion (OpenUnion innerErr) outerErr
--   , MonadRaise (OpenUnion outerErr) m
--   , MonadRescue innerErr n
--   )
--   => n a
--   -> m a
-- reraise action = try action >>= \case
--   Left  innerErr -> raise @(OpenUnion outerErr) $ consistent innerErr
--   Right value    -> pure value

-- -- | FIXME add one-liner
-- --
-- -- >>> type MyErrs = '[FooErr, BarErr]
-- -- >>> myErrs = Proxy @MyErrs
-- --
-- -- >>> :{
-- -- goesBoom :: Int -> Rescue MyErrs String
-- -- goesBoom x =
-- --   if x > 50
-- --     then return (show x)
-- --     else raiseAs @MyErrs FooErr
-- -- :}
-- --
-- -- >>> handler = catchesOpenUnion (\foo -> "Foo: " <> show foo, \bar -> "Bar:" <> show bar)
-- -- >>> rescueM myErrs (goesBoom 42) handler
-- -- RescueT (Identity (Right "Foo: FooErr"))
-- rescueM :: forall m a errs .
--   MonadRescue errs m
--   => m a
--   -> (OpenUnion errs -> a)
--   -> m a
-- rescueM action handler = rescue action (pure . handler)

-- -- | FIXME add one-liner
-- --
-- -- >>> type MyErrs = '[FooErr, BarErr]
-- -- >>> myErrs = Proxy @MyErrs
-- --
-- -- >>> :{
-- -- goesBoom :: Int -> Rescue MyErrs String
-- -- goesBoom x =
-- --   if x > 50
-- --     then return (show x)
-- --     else raiseAs @MyErrs FooErr
-- -- :}
-- --
-- -- >>> :{
-- --   handler :: OpenUnion MyErrs -> String
-- --   handler = catchesOpenUnion
-- --     ( \foo -> "Foo: " <> show foo
-- --     , \bar -> "Bar: " <> show bar
-- --     )
-- -- :}
-- --
-- -- >>> rescue' (goesBoom 42) (pure . handler) :: Rescue MyErrs String
-- -- RescueT (Identity (Right "Foo: FooErr"))
-- rescue' :: forall m a errs .
--   MonadRescue errs m
--   => m a
--   -> (OpenUnion errs -> m a)
--   -> m a
-- rescue' action handler = either handler pure =<< try action

-- | FIXME add one-liner
--
-- >>> type MyErrs = '[FooErr, BarErr]
-- >>> myErrs = Proxy @MyErrs
--
-- >>> :{
-- goesBoom :: Int -> Rescue MyErrs String
-- goesBoom x =
--   if x > 50
--     then return (show x)
--     else raiseAs @MyErrs FooErr
-- :}
--
-- >>> :{
--   handler :: OpenUnion MyErrs -> String
--   handler = catchesOpenUnion
--     ( \foo -> "Foo: " <> show foo
--     , \bar -> "Bar: " <> show bar
--     )
-- :}
--
-- >>> rescueM' (goesBoom 42) handler :: Rescue MyErrs String
-- RescueT (Identity (Right "Foo: FooErr"))
-- rescueM' :: forall m a errs .
--   MonadRescue errs m
--   => m a
--   -> (OpenUnion errs -> a)
--   -> m a
-- rescueM' action handler = rescue' action (pure . handler)

-- cleanup :: forall inner outer m resource output ignored1 ignored2 .
--   ( Contains    inner outer
--   , MonadRescue inner       m
--   , MonadRaise        outer m
--   )
--   => m resource                                  -- ^ Acquire resource
--   -> (resource -> OpenUnion inner -> m ignored1) -- ^ Cleanup exception case; The exception will be reraised
--   -> (resource -> output          -> m ignored2) -- ^ Cleanup happy path
--   -> (resource -> m output)                      -- ^ Inner action
--   -> m output
-- cleanup acquire onErr onOk action = do
--   resource <- acquire
--   try' (action resource) >>= \case
--     Right val -> do
--       _ <- onOk resource val
--       return val

--     Left err  -> do
--       _ <- onErr resource err
--       raiseTo (Proxy @outer) err

finally :: forall errs m a b .
  MonadRescue errs m
  => m a
  -> m b
  -> m a
finally action finalizer =
  try @errs action >>= \case
    Right val -> do
      _ <- finalizer
      return val

    Left err -> do
      _ <- finalizer
      raise err

-- finally' :: forall errs m a b . MonadRescue errs m => m a -> m b -> m a
-- finally' = finally (Proxy @
cleanup :: forall errs m a resource _ignored1 _ignored2 .
  MonadRescue errs m
  => m resource
  -> (resource -> OpenUnion errs -> m _ignored1)
  -> (resource ->   a            -> m _ignored2)
  -> (resource -> m a)
  -> m a
cleanup acquire onErr onOk action = do
  resource <- acquire
  try (action resource) >>= \case
    Left err -> do
      _ <- onErr resource err
      raise err

    Right output -> do
      _ <- onOk resource output
      return output

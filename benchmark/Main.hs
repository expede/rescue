{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           RIO

import           Control.Monad.Attempt
import qualified Control.Monad.Catch   as C
import           Data.WorldPeace

import           Test.Tasty.Bench

main :: IO ()
main = defaultMain
  [ bgroup "Versus manual"
      [ bench "Purely"         $ nf purely        45
      , bench "Naive"          $ nf naive         46
      , bench "Manual Fail"    $ nf manualFail    41
      , bench "Manual Success" $ nf manualSuccess 40
      , bench "Success"        $ nf success       42
      , bench "With Attempt"   $ nf withAttempt   43
      , bench "With Ensure"    $ nf withEnsure    44
      -- , bench "With Catch"     $ whnf withCatch   30
      ]
  ]

purely :: Int -> String
purely input =
  let
    val   = input + 1
    inner = val + val
  in
    show inner

naive :: Int -> Either () String
naive input = do
  let
    val   = input + 1
    inner = val + val

  return $ show inner

manualSuccess :: Int -> Either (OpenUnion '[Int]) String
manualSuccess input =
  return (Right $ input + 1) >>= \case
    Left  () ->
      return $ show ()

    Right val ->
      return (Right (val + val)) >>= \case
        Left  err   -> Left err
        Right inner -> return $ show inner

manualFail :: Int -> Either (OpenUnion '[Int]) String
manualFail input =
  return (Right (input + 1)) >>= \case
    Left () ->
      return $ show ()

    Right val ->
      return (Left (val + val)) >>= \case
        Left  err -> return $ show err
        Right ()  -> return $ show ()

success :: Int -> Either (OpenUnion '[Int]) String
success input = do
  let
    val   = input + 1
    inner = val + val

  return $ show inner

withAttempt :: Int -> Either (OpenUnion '[Int]) String
withAttempt input = do
  attempt action >>= \case
    Left err  -> return $ show err
    Right val -> return $ show val
  where
    action = do
      let val = input + 1
      _ <- raise (56 :: Int)
      return val

withEnsure :: Int -> Either (OpenUnion '[String]) String
withEnsure input = do
  val <- ensureM action
  return $ show val
  where
    action = return (Left "klkl" :: Either String Int)
    -- action = return (Right (input + 1) :: Either String Int)

withCatch :: Int -> Either SomeException String
withCatch input = do
  val <- C.catch action $ \(err :: Err) -> return $ show err
  return $ show val
  where
    action :: Either SomeException String
    action = do
      let val = input + 1
      _ <- throwM Err
      return $ show val

data Err = Err
  deriving Show

instance Exception Err

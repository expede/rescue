-- | Printable messages for exceptions

module Data.Exception.Message.Class (Message (..)) where

import           Data.Text

class Message err where
  {-# MINIMAL publicMsg #-}
  publicMsg :: err -> Text

  privateMsg :: err -> Text
  privateMsg = publicMsg

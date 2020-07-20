-- | Printable messages for exceptions

module Data.Exception.Message.Class (Message (..)) where

import           Data.Text

class Message err where
  {-# MINIMAL publicMsg #-}

  -- | A message about the exception, fit for public consumption
  publicMsg :: err -> Text

  -- | A message aout the exception wth unrestrained detail
  privateMsg :: err -> Text
  privateMsg = publicMsg

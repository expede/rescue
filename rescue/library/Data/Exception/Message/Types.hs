module Data.Exception.Message.Types (WithMessage (..)) where

import           Control.Exception
import           Data.Exception.Message.Class
import           Data.Text

-- | Attach a message to an exception, typicaly for runtime user feedback
--
-- ==== __Examples__
--
-- >>> :set -XOverloadedStrings
-- >>> show $ InvalidFormat "foo" `WithMessage` "Not a valid JSON object"
-- "InvalidFormat \"foo\" `WithMessage` \"Not a valid JSON object\""
data WithMessage err
  = err `WithMessage` Text
  deriving (Show, Eq)

instance Exception err => Exception (WithMessage err)

instance Message (WithMessage err) where
  publicMsg (WithMessage _ txt) = txt

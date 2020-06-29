{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | FIXME

module Network.HTTP.Types.Status.Error.Class (ToHttpError (..)) where

import           Data.Exception.Types

import           Network.HTTP.Types
import           Network.HTTP.Types.Status

class ToHttpError err where
  toHttpError :: err -> Status

instance ToHttpError Status where
  toHttpError = id

instance Integral n => ToHttpError n where
  toHttpError n =
    case (fromIntegral n) :: Int of
      400 -> badRequest400
      401 -> unauthorized401
      402 -> paymentRequired402
      403 -> forbidden403
      404 -> notFound404
      405 -> methodNotAllowed405
      406 -> notAcceptable406
      407 -> proxyAuthenticationRequired407
      408 -> requestTimeout408
      409 -> conflict409
      410 -> gone410
      411 -> lengthRequired411
      412 -> preconditionFailed412
      413 -> requestEntityTooLarge413
      414 -> requestURITooLong414
      415 -> unsupportedMediaType415
      416 -> requestedRangeNotSatisfiable416
      417 -> expectationFailed417
      418 -> imATeapot418
      422 -> unprocessableEntity422
      428 -> preconditionRequired428
      429 -> tooManyRequests429
      431 -> requestHeaderFieldsTooLarge431

      500 -> internalServerError500
      501 -> notImplemented501
      502 -> badGateway502
      503 -> serviceUnavailable503
      504 -> gatewayTimeout504
      505 -> httpVersionNotSupported505
      511 -> networkAuthenticationRequired511

      ok  -> error (show ok <> " is not an error status code")

instance ToHttpError (NotFound entity) where
  toHttpError NotFound = notFound404

instance ToHttpError (NotAllowed entity user) where
  toHttpError (NotAllowed _ _) = unauthorized401

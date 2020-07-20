{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Convert to an HTTP error (status code & message)

module Network.HTTP.Types.Status.Error.Class (ToHttpError (..)) where

import qualified Data.Text.Encoding as Text
import           Data.Exception.Types
import qualified Data.Exception.Message.Class as Exception

import           Network.HTTP.Types

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

instance Exception.Message (NotFound entity)
  => ToHttpError (NotFound entity) where
    toHttpError err = notFound404 `withMsg` err

instance Exception.Message (NotAllowed entity user)
  => ToHttpError (NotAllowed entity user) where
    toHttpError err = unauthorized401 `withMsg` err

instance Exception.Message (AlreadyExists entity)
  => ToHttpError (AlreadyExists entity) where
    toHttpError err = conflict409 `withMsg` err

instance Exception.Message (OutOfBounds entity index)
  => ToHttpError (OutOfBounds entity index) where
    toHttpError err = notFound404 `withMsg` err

instance ToHttpError DivideByZero where
  toHttpError err = internalServerError500 `withMsg` err

instance Exception.Message (InvalidFormat entity)
  => ToHttpError (InvalidFormat entity) where
    toHttpError err = unprocessableEntity422 `withMsg` err

withMsg :: Exception.Message err => Status -> err -> Status
withMsg status err =
  status {statusMessage = Text.encodeUtf8 $ Exception.publicMsg err}

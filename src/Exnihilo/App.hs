{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Exnihilo.App where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader.Has
import qualified Data.ByteString.Char8     as BS
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import           Network.HTTP.Client
import           Network.HTTP.Req
import           Network.HTTP.Types.Status
import           System.IO

import           Control.Monad.State
import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.Variables

newtype App a = App {getApp :: StateT Variables (ReaderT Parameters (ExceptT Error IO)) a}
  deriving newtype (Functor, Applicative, Alternative, Monad, MonadIO, MonadError Error, MonadReader Parameters, MonadState Variables)

instance MonadHttp App where
  handleHttpException = throwError . prettyHttpException
    where
      prettyHttpException = \case
        VanillaHttpException e -> case e of
          HttpExceptionRequest _ e' -> case e' of
            StatusCodeException resp _          -> ErrorUrlFetch $ mconcat ["Status code: \"", T.pack $ show  $ statusCode $ responseStatus resp, "\""]
            TooManyRedirects _                  -> ErrorUrlFetch "Too many redirects"
            OverlongHeaders                     -> ErrorUrlFetch "Overlong headers"
            ResponseTimeout                     -> ErrorUrlFetch "Response timeout"
            ConnectionTimeout                   -> ErrorUrlFetch "Connection timeout"
            ConnectionFailure _                 -> ErrorUrlFetch "Connection failure"
            InvalidStatusLine msg               -> ErrorUrlFetch $ mconcat ["Invalid status line: \"", T.pack $ BS.unpack msg, "\""]
            InvalidHeader msg                   -> ErrorUrlFetch $ mconcat ["Invalid header: \"", T.pack $ BS.unpack msg, "\""]
            InvalidRequestHeader msg            -> ErrorUrlFetch $ mconcat ["Invalid request header: \"", T.pack $ BS.unpack msg, "\""]
            InternalException _                 -> ErrorUrlFetch "Internal exception"
            ProxyConnectException _ _ status    -> ErrorUrlFetch $ mconcat ["Proxy exception: \"", T.pack $ BS.unpack $ statusMessage status, "\""]
            NoResponseDataReceived              -> ErrorUrlFetch "No data received"
            TlsNotSupported                     -> ErrorUrlFetch "Tls not supported"
            WrongRequestBodyStreamSize _ _      -> ErrorUrlFetch "Wrong request body size"
            ResponseBodyTooShort _ _            -> ErrorUrlFetch "Response body too short"
            InvalidChunkHeaders                 -> ErrorUrlFetch "Invalid chunk headers"
            IncompleteHeaders                   -> ErrorUrlFetch "Incomplete headers"
            InvalidDestinationHost msg          -> ErrorUrlFetch $ mconcat ["Invalid destination host: \"", T.pack $ BS.unpack msg, "\""]
            HttpZlibException _                 -> ErrorUrlFetch "Zlib exception"
            InvalidProxyEnvironmentVariable k v -> ErrorUrlFetch $ mconcat ["Invalid proxy environment variable (", k, " = ", v, ")"]
            ConnectionClosed                    -> ErrorUrlFetch "Conncetion closed"
            InvalidProxySettings msg            -> ErrorUrlFetch $ mconcat ["Invalid proxy settings: \"", msg, "\""]
          InvalidUrlException e' _ -> ErrorUrlInvalid $ T.pack e'
        JsonHttpException  e -> ErrorUrlFetch $ T.pack e

runApp :: MonadIO m => Parameters -> Variables -> App () -> m ()
runApp params vars = printIfError . liftIO . runExceptT . flip runReaderT params . flip evalStateT vars . getApp
  where
    printIfError m = do
      res <- m
      case res of
        Right v -> pure v
        Left e  -> liftIO $ T.hPutStrLn stderr $ prettyError e

{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Exnihilo.App where

import           Control.Monad.Except
import           Control.Monad.Reader.Has
import qualified Data.Text.IO             as T
import           System.IO

import           Exnihilo.Env
import           Exnihilo.Error

newtype App a = App {getApp :: ReaderT Env (ExceptT Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError Error, MonadReader Env)

runApp :: MonadIO m => Env -> App () -> m ()
runApp env = printIfError . liftIO . runExceptT . flip runReaderT env . getApp
  where
    printIfError m = do
      res <- m
      case res of
        Right v -> pure v
        Left e  -> liftIO $ T.hPutStrLn stderr $ prettyError e

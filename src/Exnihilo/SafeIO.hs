{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Exnihilo.SafeIO where

import           Control.Exception     (try)
import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BS
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Data.Yaml
import           Network.HTTP.Req
import           System.Directory
import           System.IO.Error
import           Text.URI

import           Exnihilo.Error

safeReadFile :: (MonadIO m, MonadError Error m) => FilePath -> m Text
safeReadFile fp  = do
  res <- liftIO tryReadFile
  case res of
    Left e  -> throwError e
    Right v -> pure v
  where
    tryReadFile :: IO (Either Error Text)
    tryReadFile = do
      res <- try $ T.readFile fp
      pure $ case res of
        Right v -> Right v
        Left e  -> Left $ handler e
    handler e
      | isDoesNotExistError e = ErrorFileReadMissing $ T.pack fp
      | isAlreadyInUseError e = ErrorFileRead        $ mconcat ["Cannot read file \"", T.pack fp, "\". File is already in use."]
      | isPermissionError   e = ErrorFileRead        $ mconcat ["Cannot read file \"", T.pack fp, "\". Permission denied."]
      | otherwise             = ErrorOther           $ mconcat ["Unknown error while reading file \"", T.pack fp, "\"."]

safeWriteFile :: (MonadIO m, MonadError Error m) => FilePath -> Text -> m ()
safeWriteFile fp content  = do
  res <- liftIO tryWriteFile
  case res of
    Left e  -> throwError e
    Right v -> pure v
  where
    tryWriteFile :: IO (Either Error ())
    tryWriteFile = do
      res <- try $ T.writeFile fp content
      pure $ case res of
        Right v -> Right v
        Left e  -> Left $ handler e
    handler e
      | isDoesNotExistError e = ErrorFileRead $ mconcat ["Cannot write file \"", T.pack fp, "\". File does not exist."]
      | isAlreadyInUseError e = ErrorFileRead $ mconcat ["Cannot write file \"", T.pack fp, "\". File is already in use."]
      | isPermissionError   e = ErrorFileRead $ mconcat ["Cannot write file \"", T.pack fp, "\". Permission denied."]
      | otherwise             = ErrorOther    $ mconcat ["Unknown error while writing file \"", T.pack fp, "\"."]

safeMakeDir :: (MonadIO m, MonadError Error m) => FilePath -> m ()
safeMakeDir fp = do
  res <- liftIO tryMakeDir
  case res of
    Left e  -> throwError e
    Right v -> pure v
  where
    tryMakeDir :: IO (Either Error ())
    tryMakeDir = do
      res <- try $ createDirectoryIfMissing True  fp
      pure $ case res of
        Right v -> Right v
        Left e  -> Left $ handler e
    handler e
      | isPermissionError e   = ErrorFileRead $ mconcat ["Cannot create directory \"", T.pack fp, "\". Permission denied."]
      | isDoesNotExistError e = ErrorFileRead $ mconcat ["Cannot create directory \"", T.pack fp, "\". Path does not exist."]
      | isFullError e         = ErrorFileRead $ mconcat ["Cannot create directory \"", T.pack fp, "\". Insufficient resources (virtual memory, process file descriptors, physical disk space, etc.)."]
      | otherwise             = ErrorOther    $ mconcat ["Unknown error while creating directory \"", T.pack fp, "\"."]

prettyYamlError :: Data.Yaml.ParseException -> Text
prettyYamlError e = T.pack ((\c -> if c == '\n' then ' ' else c) <$> prettyPrintParseException e)

safeDecodeYamlFile :: (FromJSON a, MonadIO m, MonadError Error m) => FilePath -> m a
safeDecodeYamlFile fp = safeReadFile fp >>= go
  where
    go yaml = case decodeEither' (BS.pack $ T.unpack yaml) of
      Left e  -> throwError $ ErrorFileParse (T.pack fp) $ prettyYamlError e
      Right v -> pure v

safeDecodeYaml :: (FromJSON a, MonadError Error m) => Text -> m a
safeDecodeYaml yaml = case decodeEither' (BS.pack $ T.unpack yaml) of
  Left e  -> throwError $ ErrorVariableParse $ prettyYamlError e
  Right v -> pure v

safeGetUrl :: (Monad m, MonadError Error m, MonadHttp m) => Text -> m Text
safeGetUrl url = do
  let uri = mkURI url
  r <- case uri of
         Nothing -> throwError $ ErrorUrlInvalid url
         Just validUri -> do
           let url' = useURI validUri
           case url' of
             Nothing -> throwError $ ErrorUrlInvalid url
             Just (Left (httpUrl, _))   -> req GET httpUrl  NoReqBody bsResponse mempty
             Just (Right (httpsUrl, _)) -> req GET httpsUrl NoReqBody bsResponse mempty
  pure $ T.pack $ BS.unpack $ responseBody r

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Exnihilo.SafeIO where

import           Control.Exception    (try)
import           Control.Monad.Except
import           Data.ByteString      (ByteString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Data.Yaml
import           System.Directory
import           System.IO.Error

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
      | isDoesNotExistError e = ErrorFileRead $ mconcat ["Cannot read file \"", T.pack fp, "\". File does not exist."]
      | isAlreadyInUseError e = ErrorFileRead $ mconcat ["Cannot read file \"", T.pack fp, "\". File is already in use."]
      | isPermissionError   e = ErrorFileRead $ mconcat ["Cannot read file \"", T.pack fp, "\". Permission denied."]
      | otherwise             = ErrorOther    $ mconcat ["Unknown error while reading file \"", T.pack fp, "\"."]

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

prettyYamlError :: ParseException -> Text
prettyYamlError e = T.pack ((\c -> if c == '\n' then ' ' else c) <$> prettyPrintParseException e)

safeDecodeYamlFile :: (FromJSON a, MonadIO m, MonadError Error m) => FilePath -> m a
safeDecodeYamlFile fp = do
  res <- liftIO $ decodeFileEither fp
  case res of
    Left e  -> throwError $ ErrorFileRead $ prettyYamlError e
    Right v -> pure v

safeDecodeYaml :: (FromJSON a, MonadError Error m) => ByteString -> m a
safeDecodeYaml yaml = case decodeEither' yaml of
  Left e  -> throwError $ ErrorFileRead $ prettyYamlError e
  Right v -> pure v

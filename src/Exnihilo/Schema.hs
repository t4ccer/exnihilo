{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exnihilo.Schema where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Yaml
import           Network.HTTP.Req
import           System.FilePath.Posix
import           Text.URI

import           Exnihilo.Env
import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Template
import           Exnihilo.Variables

-- TODO add type-safe wrappers for raw schema, template schema, and rendered schema
data Schema a
  = Directory a [Schema a]
  | File      a a
  deriving (Show, Functor)

instance FromJSON (Schema Text) where
  parseJSON x = parseDir x <|> parseFile x
    where
      parseDir = withObject "Schema" $ \v -> do
        dirName <- v .: "directory"
        files   <- v .: "files"
        pure $ Directory dirName files
      parseFile = withObject "Schema" $ \v -> do
        fileName <- v .: "file"
        content  <- v .: "content"
        pure $ File fileName content

traverseSchema :: Monad m => (a -> m b) -> Schema a -> m (Schema b)
traverseSchema f fs = do
  case fs of
    File fileName content -> do
      fileName' <- f fileName
      content'  <- f content
      pure $ File fileName' content'
    Directory dirName files -> do
      dirName' <- f dirName
      files' <- traverse (traverseSchema f) files
      pure $ Directory dirName' files'

parseTemplateSchema :: MonadError Error m => Schema Text -> m (Schema [Template])
parseTemplateSchema = traverseSchema parseTemplate

renderTemplateSchema :: (MonadError Error m, MonadReader r m, Has Variables r) => Schema [Template] -> m (Schema Text)
renderTemplateSchema = traverseSchema renderTemplate

saveRenderedSchema :: forall m r. (MonadIO m, MonadReader r m, Has Env r, MonadError Error m) => Schema Text -> m ()
saveRenderedSchema fs = do
  out <- asks (paramSaveLocation . envCliParams)
  safeMakeDir out
  go out fs
  where
    go :: FilePath -> Schema Text -> m ()
    go prefix f = do
      case f of
        File fileName content -> safeWriteFile (prefix </> T.unpack fileName) content
        Directory dirName files -> do
          safeMakeDir (prefix </> T.unpack dirName)
          mapM_ (go (prefix </> T.unpack dirName)) files

getRawSchema :: (MonadIO m, MonadReader r m, Has Env r, MonadError Error m, MonadHttp m) => m (Schema Text)
getRawSchema = do
  schemaLoc <- asks (paramSchemaLocation  . envCliParams)
  case schemaLoc of
    SchemaLocationPath {..} -> safeDecodeYamlFile schemaLocationPath
    SchemaLocationUrl  {..} -> do
      let uri = mkURI schemaLocationUrl
      r <- case uri of
             Nothing -> throwError $ ErrorUrlInvalid schemaLocationUrl
             Just uri' -> do
               let url = useURI uri'
               case url of
                 Nothing -> throwError $ ErrorUrlInvalid schemaLocationUrl
                 Just (Left (url', _)) -> req GET url' NoReqBody bsResponse mempty
                 Just (Right (url', _)) -> req GET url' NoReqBody bsResponse mempty
      let body = responseBody r
      safeDecodeYaml body

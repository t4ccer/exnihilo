{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Exnihilo.Schema where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Yaml
import           Network.HTTP.Req
import           System.FilePath.Posix

import           Data.String
import           Exnihilo.Env
import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Template
import           Exnihilo.Variables

data Schema a
  = Directory a [Schema a]
  | File      a a
  deriving (Show, Functor)

newtype RawSchema = RawSchema (Schema Text)
  deriving newtype FromJSON

newtype TemplateSchema = TemplateSchema (Schema [Template])

newtype RenderedSchema = RenderedSchem (Schema Text)

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

parseRawSchema :: MonadError Error m => RawSchema -> m TemplateSchema
parseRawSchema (RawSchema schema) = TemplateSchema <$> traverseSchema parseTemplate schema

renderTemplateSchema :: (MonadError Error m, MonadReader r m, Has Variables r) => TemplateSchema -> m RenderedSchema
renderTemplateSchema (TemplateSchema schema) = RenderedSchem <$> traverseSchema renderTemplate schema

saveRenderedSchema :: forall m r. (MonadIO m, MonadReader r m, Has Env r, MonadError Error m) => RenderedSchema -> m ()
saveRenderedSchema (RenderedSchem schema) = do
  out <- asks (paramSaveLocation . envCliParams)
  safeMakeDir out
  go out schema
  where
    go :: FilePath -> Schema Text -> m ()
    go prefix f = do
      case f of
        File fileName content -> safeWriteFile (prefix </> T.unpack fileName) content
        Directory dirName files -> do
          safeMakeDir (prefix </> T.unpack dirName)
          mapM_ (go (prefix </> T.unpack dirName)) files

getRawSchema :: forall m r. (MonadIO m, MonadReader r m, Has Env r, MonadError Error m, MonadHttp m, Alternative m) => m RawSchema
getRawSchema = do
  schemaLoc <- asks (paramSchemaLocation  . envCliParams)
  case schemaLoc of
    SchemaLocationPath {..} ->
      tryGetSchema safeDecodeYamlFile schemaLocationPath
    SchemaLocationUrl  {..} ->
      tryGetSchema (safeDecodeYaml <=< safeGetUrl) schemaLocationUrl
    SchemaLocationGithub {..} -> do
      let url
            =  "https://raw.githubusercontent.com/"
                   <> schemaLocationGithubRepo
            <> "/" <> schemaLocationGithubRef
            <> "/" <> schemaLocationGithubPath
      tryGetSchema (safeDecodeYaml <=< safeGetUrl) url
    where
      tryGetSchema :: (Semigroup a, IsString a) => (a -> m RawSchema) -> a -> m RawSchema
      tryGetSchema f x = firstSuccess f $ possiblePaths x
      possiblePaths xs =
        [ xs
        , xs <> ".yaml"
        , xs <> ".yml"
        , xs <> "/exnihilo.yaml"
        , xs <> "/exnihilo.yml"
        , xs <> "/.exnihilo"
        , xs <> "/.exnihilo.yaml"
        , xs <> "/.exnihilo.yml"
        ]
      firstSuccess :: Alternative f => (b -> f a) -> [b] -> f a
      firstSuccess _ []     = empty
      firstSuccess f (x:xs) = f x <|> firstSuccess f xs

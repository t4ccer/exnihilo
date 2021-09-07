{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exnihilo.Schema where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Data.Text                (Text)
import           Data.Yaml
import           System.FilePath.Posix

import           Exnihilo.Env
import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Template
import           Exnihilo.Variables

-- TODO templates in file names
data Schema a
  = Directory FilePath [Schema a]
  | File      FilePath a
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

parseTemplateSchema :: MonadError Error m => Schema Text -> m (Schema [Template])
parseTemplateSchema fs = do
  case fs of
    File fileName content -> do
      contentTemplate <- parseTemplate content
      pure $ File fileName contentTemplate
    Directory dirName files -> do
      fileTemplates <- traverse parseTemplateSchema files
      pure $ Directory dirName fileTemplates

renderTemplateSchema :: (MonadError Error m, MonadReader r m, Has Variables r) => Schema [Template] -> m (Schema Text)
renderTemplateSchema fs = do
  case fs of
    File fileName content -> do
      renderedContent <- renderTemplate content
      pure $ File fileName renderedContent
    Directory dirName files -> do
      renderedFiles <- traverse renderTemplateSchema files
      pure $ Directory dirName renderedFiles

saveRenderedSchema :: forall m r. (MonadIO m, MonadReader r m, Has Env r, MonadError Error m) => Schema Text -> m ()
saveRenderedSchema fs = do
  out <- asks (paramSaveLocation . envCliParams)
  safeMakeDir out
  go out fs
  where
    go :: FilePath -> Schema Text -> m ()
    go prefix f = do
      case f of
        File fileName content -> safeWriteFile (prefix </> fileName) content
        Directory dirName files -> do
          safeMakeDir (prefix </> dirName)
          mapM_ (go (prefix </> dirName)) files

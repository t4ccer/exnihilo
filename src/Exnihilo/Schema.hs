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
import qualified Data.Map                 as M
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Yaml
import           Network.HTTP.Req
import           System.FilePath.Posix

import           Control.Monad.State
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

newtype TemplateSchema = TemplateSchema {getTemplateSchema :: Schema Template}

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

renderTemplateSchema :: (MonadError Error m, MonadState Variables m) => TemplateSchema -> m RenderedSchema
renderTemplateSchema (TemplateSchema schema) = RenderedSchem <$> traverseSchema renderTemplate schema

saveRenderedSchema :: forall m. (MonadIO m, MonadReader Parameters m, MonadError Error m) => RenderedSchema -> m ()
saveRenderedSchema (RenderedSchem schema) = do
  out <- asks paramSaveLocation
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

toList :: Monoid a => Schema a -> [(a,a)]
toList (File fp content)    = [(fp, content)]
toList (Directory fp files) = (fp, mempty) : concatMap toList files

schemaUsedVariables :: TemplateSchema -> [Text]
schemaUsedVariables = concatMap (concatMap requiredVariables . \(x,y) -> [x, y]) . toList . getTemplateSchema

schemaMissingVariables :: (MonadState Variables m) => TemplateSchema -> m [Text]
schemaMissingVariables schema = do
  declaredNames <- gets (S.fromList . M.keys . getVariables)
  let usedNames = S.fromList $ schemaUsedVariables schema
  pure $ S.toList $ S.difference usedNames declaredNames

tryGetMissingVariables :: (MonadIO m, MonadState Variables m, MonadReader Parameters m) => TemplateSchema -> m ()
tryGetMissingVariables schema = do
  Parameters{..} <- ask
  unless paramNoInteractive $ do
    missing <- schemaMissingVariables schema
    askForMissingVariables missing

getRawSchema :: forall m. (MonadIO m, MonadReader Parameters m, MonadError Error m, MonadHttp m) => m RawSchema
getRawSchema = do
  schemaLoc <- asks paramSchemaLocation
  case schemaLoc of
    SchemaLocationPath {..} ->
      tryGetSchema  safeDecodeYamlFile (T.pack schemaLocationPath) schemaLocationPath
    SchemaLocationUrl  {..} ->
      tryGetSchema (safeDecodeYaml <=< safeGetUrl) schemaLocationUrl schemaLocationUrl
    SchemaLocationGithub {..} -> do
      let url
            =  "https://raw.githubusercontent.com/"
                   <> schemaLocationGithubRepo
            <> "/" <> schemaLocationGithubRef
            <> "/" <> schemaLocationGithubPath
      tryGetSchema (safeDecodeYaml <=< safeGetUrl) url url
    where
      tryGetSchema f fp x = firstSuccess f fp (possiblePaths x)
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
      firstSuccess _ fp [] = throwError $ ErrorFileReadMissing fp
      firstSuccess f fp (x:xs) = f x `catchError` (\e -> case e of
                                                   ErrorFileReadMissing _ -> firstSuccess f fp xs
                                                   _                      -> throwError e)

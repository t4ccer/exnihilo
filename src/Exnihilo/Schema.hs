{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Exnihilo.Schema where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Control.Monad.State
import           Data.Coerce              (coerce)
import           Data.Foldable            (traverse_)
import qualified Data.Map                 as M
import           Data.Maybe
import qualified Data.Set                 as S
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Yaml
import           Network.HTTP.Req
import           System.FilePath.Posix

import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Template
import           Exnihilo.Variables

data Schema a
  = Directory
  { directoryName            :: a
  , directoryFiles           :: [Schema a]
  , directoryCreateCondition :: Maybe Text
  }
  | File
  { fileName            :: a
  , fileContent         :: a
  , fileCreateCondition :: Maybe Text
  }
  deriving (Show, Functor)

newtype RawSchema = RawSchema (Schema Text)
  deriving newtype FromJSON

newtype TemplateSchema = TemplateSchema {getTemplateSchema :: Schema Template}

newtype RenderedSchema = RenderedSchem (Schema Text)

instance FromJSON (Schema Text) where
  parseJSON x = parseDir x <|> parseFile x
    where
      parseDir = withObject "Schema" $ \v -> do
        directoryName  <- v .: "directory"
        directoryFiles <- v .: "files"
        directoryCreateCondition <- v .:? "condition"
        pure $ Directory {..}
      parseFile = withObject "Schema" $ \v -> do
        fileName    <- v .: "file"
        fileContent <- v .: "content"
        fileCreateCondition <- v .:? "condition"
        pure $ File {..}

traverseSchema :: Monad m => (a -> m b) -> Schema a -> m (Schema b)
traverseSchema f fs =
  case fs of
    File{..} -> do
      fileName' <- f fileName
      fileContent'  <- f fileContent
      pure $ File fileName' fileContent' fileCreateCondition
    Directory{..} -> do
      directoryName' <- f directoryName
      directoryFiles' <- traverse (traverseSchema f) directoryFiles
      pure $ Directory directoryName' directoryFiles' directoryCreateCondition

parseRawSchema :: MonadError Error m => RawSchema -> m TemplateSchema
parseRawSchema (RawSchema schema) = TemplateSchema <$> traverseSchema parseTemplate schema

typeCheckTemplateSchema :: (MonadError Error m, MonadState Variables m) => TemplateSchema -> m ()
typeCheckTemplateSchema (TemplateSchema schema) =
  case schema of
    File{..} -> do
      case fileCreateCondition of
        Nothing -> pure ()
        Just name -> do
          v <- lookupVariable name
          case v of
            VarBool _ -> pure ()
            _         -> throwError $ ErrorTypeCheck "Create condition must be bool"
    Directory{..} -> do
      case directoryCreateCondition of
        Nothing -> pure ()
        Just name -> do
          v <- lookupVariable name
          case v of
            VarBool _ -> traverse_ typeCheckTemplateSchema $ coerce @_ @[TemplateSchema] directoryFiles
            _         -> throwError $ ErrorTypeCheck "Create condition must be bool"

renderTemplateSchema :: (MonadError Error m, MonadState Variables m) => TemplateSchema -> m RenderedSchema
renderTemplateSchema (TemplateSchema schema) = RenderedSchem <$> traverseSchema renderTemplate schema

saveRenderedSchema :: forall m. (MonadIO m, MonadReader Parameters m, MonadError Error m, MonadState Variables m) => RenderedSchema -> m ()
saveRenderedSchema (RenderedSchem schema) = do
  out <- asks paramSaveLocation
  safeMakeDir out
  go out schema
  where
    whenVar k a =
      case k of
        Nothing -> a
        Just k' -> do
          v <- lookupVariable k'
          case v of
            VarBool True  -> a
            VarBool False -> pure ()
            _             -> throwError $ ErrorTypeCheck "Create condition must be bool"
    go :: FilePath -> Schema Text -> m ()
    go prefix f =
      case f of
        File{..} -> whenVar fileCreateCondition $ safeWriteFile (prefix </> T.unpack fileName) fileContent
        Directory{..} -> whenVar directoryCreateCondition $ do
          safeMakeDir (prefix </> T.unpack directoryName)
          mapM_ (go (prefix </> T.unpack directoryName)) directoryFiles

toList :: Monoid a => Schema a -> [(a,a)]
toList File{..}      = [(fileName, fileContent)]
toList Directory{..} = (directoryName, mempty) : concatMap toList directoryFiles

schemaUsedVariables :: TemplateSchema -> [Text]
schemaUsedVariables schema = mapMaybe getVariable $ concatMap getTemplateAst $ go schema
  where
    go (TemplateSchema s) = case s of
      File{..} ->
        [ fileName
        , fileContent
        , maybe (Template []) (Template . pure . TemplateVariable) fileCreateCondition
        ]
      Directory{..} ->
        [ directoryName
        , maybe (Template []) (Template . pure . TemplateVariable) directoryCreateCondition
        ] <> concatMap (go . TemplateSchema) directoryFiles
    getVariable (TemplateVariable xs) = Just xs
    getVariable _                     = Nothing

schemaMissingVariables :: (MonadState Variables m) => TemplateSchema -> m [Text]
schemaMissingVariables schema = do
  declaredNames <- gets (S.fromList . M.keys . getVariables)
  let usedNames = S.fromList $ schemaUsedVariables schema
  pure $ S.toList $ S.difference usedNames declaredNames

tryGetMissingVariables :: (MonadIO m, MonadState Variables m, MonadReader Parameters m, MonadError Error m) => TemplateSchema -> m ()
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

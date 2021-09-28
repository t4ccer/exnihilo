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

module Exnihilo.Schema
  ( Schema(..), RawSchema, TemplateSchema, RenderedSchema
  , FilesSchema(..)
  , getRawSchema, parseRawSchema, typeCheckTemplateSchema
  , renderTemplateSchema, saveRenderedSchema, handleMissingVariables
  , runPreSaveHooks
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Control.Monad.State
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
import           Exnihilo.Hook
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Template
import           Exnihilo.Variables

data Schema a
  = Schema
  { schemaFiles        :: FilesSchema a
  , schemaPostSaveHook :: [Hook a]
  , schemaPreSaveHook  :: [Hook a]
  }
  deriving (Show, Functor)

instance FromJSON (Schema Text) where
  parseJSON = withObject "Schema" $ \v -> do
    schemaFiles        <- v .: "files"
    schemaPostSaveHook <- fromMaybe [] <$> v .:? "postSaveHook"
    schemaPreSaveHook  <- fromMaybe [] <$> v .:? "preSaveHook"
    pure Schema{..}

data FilesSchema a
  = Directory
  { directoryName            :: a
  , directoryFiles           :: [FilesSchema a]
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

newtype TemplateSchema = TemplateSchema (Schema Template)

newtype RenderedSchema = RenderedSchem (Schema Text)

instance FromJSON (FilesSchema Text) where
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

traverseFilesSchema :: Monad m => (a -> m b) -> FilesSchema a -> m (FilesSchema b)
traverseFilesSchema f schema =
  case schema of
    File{..} -> do
      fileName' <- f fileName
      fileContent'  <- f fileContent
      pure $ File fileName' fileContent' fileCreateCondition
    Directory{..} -> do
      directoryName' <- f directoryName
      directoryFiles' <- traverse (traverseFilesSchema f) directoryFiles
      pure $ Directory directoryName' directoryFiles' directoryCreateCondition

traverseHooks :: Monad m => (a -> m b) -> [Hook a] -> m [Hook b]
traverseHooks f hooks = do
  let vals = fmap hookCommand hooks
  newVals <- traverse f vals
  let newHooks = zipWith (\n h -> h{hookCommand = n}) newVals hooks
  pure newHooks

traverseSchema :: Monad m => (a1 -> m a2) -> Schema a1 -> m (Schema a2)
traverseSchema f schema@Schema{..} = do
  fs <- traverseFilesSchema f schemaFiles
  newPostSaveHook <- traverseHooks f schemaPostSaveHook
  newPreSaveHook  <- traverseHooks f schemaPreSaveHook
  pure $ schema
    { schemaFiles = fs
    , schemaPostSaveHook = newPostSaveHook
    , schemaPreSaveHook = newPreSaveHook
    }

parseRawSchema :: MonadError Error m => RawSchema -> m TemplateSchema
parseRawSchema (RawSchema schema) = TemplateSchema <$> traverseSchema parseTemplate schema

typeCheckTemplateSchema :: (MonadError Error m, MonadState Variables m) => TemplateSchema -> m ()
typeCheckTemplateSchema (TemplateSchema Schema{..}) = typeCheckFiles schemaFiles
  where
    typeCheckFiles fs =
      case fs of
        File{..} -> do
          case fileCreateCondition of
            Nothing -> pure ()
            Just name -> do
              v <- lookupVariable name
              case v of
                VarBool _ -> pure ()
                _         -> throwError $ ErrorCreateTypeCheck ""
        Directory{..} -> do
          case directoryCreateCondition of
            Nothing -> pure ()
            Just name -> do
              v <- lookupVariable name
              case v of
                VarBool _ -> traverse_ typeCheckFiles directoryFiles
                _         -> throwError $ ErrorCreateTypeCheck ""

renderTemplateSchema :: (MonadError Error m, MonadState Variables m) => TemplateSchema -> m RenderedSchema
renderTemplateSchema (TemplateSchema schema) = RenderedSchem <$> traverseSchema renderTemplate schema

saveRenderedSchema :: forall m. (MonadIO m, MonadReader Parameters m, MonadError Error m, MonadState Variables m) => RenderedSchema -> m ()
saveRenderedSchema (RenderedSchem Schema{..}) = do

  out <- asks paramSaveLocation
  safeMakeDir out
  go out schemaFiles
  runHooks schemaPostSaveHook
  where
    whenVar k a =
      case k of
        Nothing -> a
        Just k' -> do
          v <- lookupVariable k'
          case v of
            VarBool True  -> a
            VarBool False -> pure ()
            _             -> throwError $ ErrorCreateTypeCheck ""
    go :: FilePath -> FilesSchema Text -> m ()
    go prefix f =
      case f of
        File{..} -> whenVar fileCreateCondition $ safeWriteFile (prefix </> T.unpack fileName) fileContent
        Directory{..} -> whenVar directoryCreateCondition $ do
          safeMakeDir (prefix </> T.unpack directoryName)
          mapM_ (go (prefix </> T.unpack directoryName)) directoryFiles

schemaUsedVariables :: TemplateSchema -> [Text]
schemaUsedVariables (TemplateSchema Schema{..}) = filesVars <> postSaveHookVars <> preSaveHookVars
  where
    postSaveHookVars = getFromHook schemaPostSaveHook
    preSaveHookVars  = getFromHook schemaPreSaveHook
    filesVars = mapMaybe getVariable $ concatMap getTemplateAst $ go schemaFiles
    getFromHook = catMaybes . concatMap (fmap getVariable . getTemplateAst . hookCommand)
    go :: FilesSchema Template -> [Template]
    go s = case s of
      File{..} ->
        [ fileName
        , fileContent
        , maybe (Template []) (Template . pure . TemplateVariable) fileCreateCondition
        ]
      Directory{..} ->
        [ directoryName
        , maybe (Template []) (Template . pure . TemplateVariable) directoryCreateCondition
        ] <> concatMap go directoryFiles
    getVariable (TemplateVariable xs) = Just xs
    getVariable _                     = Nothing

schemaMissingVariables :: (MonadState Variables m) => TemplateSchema -> m [Text]
schemaMissingVariables schema = do
  declaredNames <- gets (S.fromList . M.keys . getVariables)
  let usedNames = S.fromList $ schemaUsedVariables schema
  pure $ S.toList $ S.difference usedNames declaredNames

handleMissingVariables :: (MonadIO m, MonadState Variables m, MonadReader Parameters m, MonadError Error m) => TemplateSchema -> m ()
handleMissingVariables schema = do
  Parameters{..} <- ask
  missing <- schemaMissingVariables schema
  unless (null missing) $
    if paramNoInteractive
      then throwError $ ErrorMissingVariable $ T.intercalate ", " missing
      else getMissingVariables getMissingVariableInteractive missing

getRawSchema :: forall m. (MonadIO m, MonadReader Parameters m, MonadError Error m, MonadHttp m, MonadState Variables m) => m RawSchema
getRawSchema = do
  RawSchema <$> getSchema
    where
      getSchema = do
        schemaLoc <- asks paramSchemaLocation
        case schemaLoc of
          SchemaLocationPath {..} ->
            tryGetSchema  safeDecodeYamlFile (T.pack schemaLocationPath) schemaLocationPath
          SchemaLocationUrl  {..} ->
            tryGetSchema (safeDecodeYaml <=< safeGetUrl) schemaLocationUrl schemaLocationUrl
          SchemaLocationGithub {..} -> do
            let url
                  = "https://raw.githubusercontent.com/"
                         <> schemaLocationGithubRepo
                  <> "/" <> schemaLocationGithubRef
                  <> "/" <> schemaLocationGithubPath
            tryGetSchema (safeDecodeYaml <=< safeGetUrl) url url
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
      firstSuccess f fp (x:xs) =
        f x `catchError` (\e -> case e of
                                  ErrorFileReadMissing _               -> firstSuccess f fp xs
                                  ErrorUrlFetch "Status code: \"404\"" -> firstSuccess f fp xs
                                  _                                    -> throwError e)

runPreSaveHooks :: (MonadIO m, MonadError Error m, MonadState Variables m, MonadReader Parameters m) => RawSchema -> m ()
runPreSaveHooks (RawSchema schema) = do
  asks paramSaveLocation >>= safeMakeDir
  runHooks $ schemaPreSaveHook schema

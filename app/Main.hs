{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Applicative
import           Control.Exception        (try)
import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Data.Map                 (Map)
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import           Data.Yaml
import           GHC.Generics
import           Options.Applicative
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.IO
import           System.IO.Error

data Error
  = ErrorMissingVariable Text
  | ErrorFileRead Text
  | ErrorFileWrite Text
  | ErrorOther Text
  deriving (Show)

data Template
  = TemplateVariable Text
  | TemplateConstant Text
  deriving (Show)

newtype UnparsedTemplate = UnparsedTemplate {getUnparsedTemplate :: Text}
  deriving (Show, Eq)

-- TODO templates in file names
data FileStruct a
  = Directory FilePath [FileStruct a]
  | File      FilePath a
  deriving (Show, Functor)

instance FromJSON (FileStruct Text) where
  parseJSON x = parseDir x <|> parseFile x
    where
      parseDir = withObject "FileStruct" $ \v -> do
        dirName <- v .: "directory"
        files   <- v .: "files"
        pure $ Directory dirName files
      parseFile = withObject "FileStruct" $ \v -> do
        fileName <- v .: "file"
        content  <- v .: "content"
        pure $ File fileName content

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

-- TODO add error handling
safeMakeDir :: MonadIO m => FilePath -> m ()
safeMakeDir = liftIO . createDirectoryIfMissing True

safeDecodeYamlFile :: (FromJSON a, MonadIO m, MonadError Error m) => FilePath -> m a
safeDecodeYamlFile fp = do
  res <- liftIO $ decodeFileEither fp
  case res of
    Left e  -> throwError $ ErrorFileRead $ T.pack $ fmap (\c -> if c == '\n' then ' ' else c) $ prettyPrintParseException e
    Right v -> pure v

newtype Variables = Variables {getVariables :: Map Text Text}

data Parameters = Parameters
  { paramVariableFile :: FilePath
  , paramTemplateFile :: FilePath
  , paramSaveLocation :: FilePath
  }

data  Env = Env
  { envVariables :: Variables
  , envCliParams :: Parameters
  }
  deriving (Generic, Has Variables, Has Parameters)

newtype ReleaseApp a = ReleaseApp {getReleaseApp :: ReaderT Env (ExceptT Error IO) a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError Error, MonadReader Env)

getParams :: (MonadIO m) => m Parameters
getParams = liftIO $ execParser $ info (helper <*> opts) fullDesc
  where
    opts = do
      paramVariableFile <- strOption (long "vars"     <> metavar "FILE" <> help "")
      paramTemplateFile <- strOption (long "template" <> metavar "FILE" <> help "")
      paramSaveLocation <- strOption (long "output"   <> metavar "FILE" <> help "")
      pure Parameters{..}

runReleaseApp :: MonadIO m => Env -> ReleaseApp () -> m ()
runReleaseApp env = printError . liftIO . runExceptT . flip runReaderT env . getReleaseApp
  where
    printError m = do
      res <- m
      case res of
        Right v -> pure v
        Left e  -> liftIO $ T.hPutStrLn stderr $ prettyError e

prettyError :: Error -> Text
prettyError = \case
  ErrorFileRead e        -> mconcat ["[Error] File read error. ", e]
  ErrorMissingVariable e -> mconcat ["[Error] Missing variable in schema. ", e]
  ErrorFileWrite e       -> mconcat ["[Error] File write error. ", e]
  ErrorOther e           -> mconcat ["[Error] Unknown error occured. ", e]

saveRenderedSchema :: forall m r. (MonadIO m, MonadReader r m, Has Env r, MonadError Error m) => FileStruct Text -> m ()
saveRenderedSchema fs = do
  out <- asks (paramSaveLocation . envCliParams)
  safeMakeDir out
  go out fs
  where
    go :: FilePath -> FileStruct Text -> m ()
    go prefix f = do
      case f of
        File fileName content -> safeWriteFile (prefix </> fileName) content
        Directory dirName files -> do
          safeMakeDir (prefix </> dirName)
          mapM_ (go (prefix </> dirName)) files

lookupVariable :: Variables -> Text -> Maybe Text
lookupVariable (Variables vars) key = M.lookup key vars

-- TODO add variable support
parseTemplate :: Monad m => Text -> m [Template]
parseTemplate = pure . pure . TemplateConstant

parseTemplateSchema :: MonadError Error m => FileStruct Text -> m (FileStruct [Template])
parseTemplateSchema fs = do
  case fs of
    File fileName content -> do
      contentTemplate <- parseTemplate content
      pure $ File fileName contentTemplate
    Directory dirName files -> do
      fileTemplates <- traverse parseTemplateSchema files
      pure $ Directory dirName fileTemplates

renderTemplate :: forall m r. (MonadReader r m, Has Variables r, MonadError Error m) => [Template] -> m Text
renderTemplate templ = do
  vars :: Variables <- ask
  let
    go :: Template -> m Text
    go = \case
       (TemplateVariable var) -> case lookupVariable vars var of
         Nothing -> throwError $ ErrorMissingVariable var
         Just v  -> pure v
       (TemplateConstant cons) -> pure cons
  T.concat <$> traverse go templ

renderTemplateSchema :: (MonadError Error m, MonadReader r m, Has Variables r) => FileStruct [Template] -> m (FileStruct Text)
renderTemplateSchema fs = do
  case fs of
    File fileName content -> do
      renderedContent <- renderTemplate content
      pure $ File fileName renderedContent
    Directory dirName files -> do
      renderedFiles <- traverse renderTemplateSchema files
      pure $ Directory dirName renderedFiles

-- TODO make it efficient, add errors
varsFromLine :: forall m. Monad m => Text -> m (Text, Text)
varsFromLine = go mempty
  where
    go :: Text -> Text -> m (Text, Text)
    go acc inp = if T.head inp == '='
      then pure (acc, T.tail inp)
      else go (acc <> T.singleton (T.head inp)) (T.tail inp)

varsFromFile :: (MonadIO m, MonadError Error m) => FilePath -> m Variables
varsFromFile fp = do
  content <- safeReadFile fp
  vars <- mapM varsFromLine $ T.lines content
  pure $ Variables $ M.fromList vars

run :: (MonadIO m) => Env -> m ()
run env = runReleaseApp env $
      asks (paramTemplateFile . envCliParams)
  >>= safeDecodeYamlFile
  >>= parseTemplateSchema
  >>= renderTemplateSchema
  >>= saveRenderedSchema

runInit :: (MonadIO m) => m (Either Error Env)
runInit = runExceptT $ do
  params <- getParams
  vars   <- varsFromFile $ paramVariableFile params
  pure $ Env vars params

main :: IO ()
main = do
  res <- runInit
  case res of
    Left e    -> T.hPutStrLn stderr $ prettyError e
    Right env -> run env

debug :: String -> IO ()
debug = flip withArgs main . words

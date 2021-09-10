{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Exnihilo.Parameters
  ( Parameters(..), VersionParameters(..)
  , Mode(..), SchemaLocation(..)
  , getParams
  ) where

import           Control.Monad.Except
import           Data.Maybe
import           Data.Text            (Text)
import           Options.Applicative

data Parameters = Parameters
  { paramVariableFile   :: FilePath
  , paramSchemaLocation :: SchemaLocation
  , paramSaveLocation   :: FilePath
  }

newtype VersionParameters = VersionParameters
  { paramVersionNumeric :: Bool
  }

data Mode
 = ModeVersion VersionParameters
 | ModeCreate Parameters

data SchemaLocation
  = SchemaLocationPath
  { schemaLocationPath :: FilePath
  }
  | SchemaLocationUrl
  { schemaLocationUrl :: Text
  }
  | SchemaLocationGithub
  { schemaLocationGithubRepo :: Text
  , schemaLocationGithubRef  :: Text
  , schemaLocationGithubPath :: Text
  }

getParams :: (MonadIO m) => m Mode
getParams = liftIO $ customExecParser (prefs showHelpOnError) $ info (helper <*> opts) fullDesc

opts :: Parser Mode
opts = hsubparser $ mconcat
  [ command "version" (info (ModeVersion <$> versionOpts) (progDesc "Print version and exit"))
  , command "create"  (info (ModeCreate  <$> createOpts)  (progDesc "Create project from schema"))
  ]

versionOpts :: Parser VersionParameters
versionOpts = do
  paramVersionNumeric <- switch (long "numeric" <> help "Print only version number")
  pure VersionParameters{..}

createOpts :: Parser Parameters
createOpts = do
  paramVariableFile <- strOption (short 'v' <> long "variables"   <> metavar "FILE" <> help "Path to file with variables")
  paramSchemaLocation <- schemaOption
  paramSaveLocation <- strOption (short 'd' <> long "destination" <> metavar "PATH" <> help "Path to new project")
  pure Parameters{..}

schemaOption :: Parser SchemaLocation
schemaOption
  =   (do
         schemaLocationPath <- strOption (short 's' <> long "schema" <> metavar "PATH" <> help "Path to schema file")
         pure SchemaLocationPath {..})
  <|> (do
        schemaLocationUrl <- strOption (short 'u' <> long "url"    <> metavar "URL"  <> help "Url to schema file")
        pure SchemaLocationUrl {..})
  <|> (do
        schemaLocationGithubRepo <- strOption (short 'g' <> long "github" <> metavar "REPO"  <> help "format: user/repo")
        schemaLocationGithubRef  <- withDefault "main" $ strOption (long "ref" <> metavar "REF"  <> help "Branch or tag. Default: main")
        schemaLocationGithubPath <- strOption (short 's' <> long "schema" <> metavar "PATH" <> help "Path to schema file")
        pure SchemaLocationGithub{..})

withDefault :: a -> Parser a -> Parser a
withDefault d opt = fromMaybe d <$> optional opt

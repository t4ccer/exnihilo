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

import qualified Data.Map             as M
import           Exnihilo.Variables

data Parameters = Parameters
  { paramVariableFile        :: Maybe FilePath
  , paramSchemaLocation      :: SchemaLocation
  , paramSaveLocation        :: FilePath
  , paramNoInteractive       :: Bool
  , paramVariableOverrides   :: Variables
  , paramNoImplicitVariables :: Bool
  }
 deriving (Show)

newtype VersionParameters = VersionParameters
  { paramVersionNumeric :: Bool
  }
 deriving (Show)

data Mode
 = ModeVersion VersionParameters
 | ModeCreate Parameters
 deriving (Show)

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
 deriving (Show)

getParams :: (MonadIO m) => m Mode
getParams = liftIO $ customExecParser (prefs showHelpOnError) $ info (helper <*> opts) fullDesc

opts :: Parser Mode
opts = versionOpts <|> createOpts

versionOpts :: Parser Mode
versionOpts = do
  _ <- switch (long "version" <> help "Print version")
  paramVersionNumeric <- switch (long "numeric" <> help "Print only version number")
  pure $ ModeVersion VersionParameters{..}

createOpts :: Parser Mode
createOpts = do
  paramVariableFile        <- optional $ strOption (long "variables"   <> metavar "FILE" <> help "Path to file with variables")
  paramSchemaLocation      <- schemaOption
  paramSaveLocation        <- strOption (short 'd' <> long "destination" <> metavar "PATH" <> help "Path to new project")
  paramNoInteractive       <- flag False True (long "no-interactive" <> help "Disable interactions. Return with error on missing variable instead of asking")
  paramVariableOverrides   <- fromList <$> many varOpts
  paramNoImplicitVariables <- flag False True  (long "no-implicit" <> help "Do not add implicit variables, like current date, etc.")
  pure $ ModeCreate Parameters{..}

varOpts :: Parser (Text, Variable)
varOpts = do
  k <- strOption (short 'k' <> long "key" <> metavar "KEY" <> help "Variable name to overrite")
  v <- strOption (short 'v' <> long "value" <> metavar "VAL" <> help "Variable value to overrite")
  pure $ either (const (k, VarString v)) (head . M.toList . getVariables) $ runExcept $ parseVariable k v

schemaOption :: Parser SchemaLocation
schemaOption
  =   (do
         schemaLocationPath      <- strOption (short 's' <> long "schema" <> metavar "PATH" <> help "Path to schema file")
         pure SchemaLocationPath {..})
  <|> (do
        schemaLocationUrl        <- strOption (short 'u' <> long "url"    <> metavar "URL"  <> help "Url to schema file")
        pure SchemaLocationUrl {..})
  <|> (do
        schemaLocationGithubRepo <- strOption (short 'g' <> long "github" <> metavar "REPO"  <> help "format: user/repo")
        schemaLocationGithubRef  <- withDefault "main" $ strOption (long "ref" <> metavar "REF"  <> help "Branch or tag. Default: main")
        schemaLocationGithubPath <- strOption (short 's' <> long "schema" <> metavar "PATH" <> help "Path to schema file")
        pure SchemaLocationGithub{..})

withDefault :: a -> Parser a -> Parser a
withDefault d opt = fromMaybe d <$> optional opt

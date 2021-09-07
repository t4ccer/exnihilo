{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Exnihilo.Parameters where

import           Control.Monad.Except
import           Options.Applicative

-- TODO add template url
-- TODO add template github repo
data Parameters = Parameters
  { paramVariableFile :: FilePath
  , paramTemplateFile :: FilePath
  , paramSaveLocation :: FilePath
  }

newtype VersionParameters = VersionParameters
  { paramVersionNumeric :: Bool
  }

data Mode
 = ModeVersion VersionParameters
 | ModeCreate Parameters

getParams :: (MonadIO m) => m Mode
getParams = liftIO $ customExecParser (prefs showHelpOnError) $ info (helper <*> opts) fullDesc
  where
    opts = hsubparser $ mconcat
      [ command "version" (info (ModeVersion <$> versionOpts) (progDesc "Print version and exit"))
      , command "create"  (info (ModeCreate  <$> createOpts)  (progDesc "Create project from schema"))
      ]
    versionOpts = do
      paramVersionNumeric <- switch (long "numeric" <> help "Print only version number")
      pure VersionParameters{..}
    createOpts = do
      paramVariableFile <- strOption (short 'v' <> long "variables"   <> metavar "FILE" <> help "Path to file with variables")
      paramTemplateFile <- strOption (short 's' <> long "schema"      <> metavar "PATH" <> help "Path to schema file")
      paramSaveLocation <- strOption (short 'd' <> long "destination" <> metavar "PATH" <> help "Path to new project")
      pure Parameters{..}

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Exnihilo.Parameters where

import           Control.Monad.Except
import           Options.Applicative

-- TODO add template url
-- TODO add template github repo
-- TODO add version param
data Parameters = Parameters
  { paramVariableFile :: FilePath
  , paramTemplateFile :: FilePath
  , paramSaveLocation :: FilePath
  }

getParams :: (MonadIO m) => m Parameters
getParams = liftIO $ execParser $ info (helper <*> opts) fullDesc
  where
    opts = do
      paramVariableFile <- strOption (short 'v' <> long "variables"   <> metavar "FILE" <> help "Path to file with variables")
      paramTemplateFile <- strOption (short 's' <> long "schema"      <> metavar "PATH" <> help "Path to schema file")
      paramSaveLocation <- strOption (short 'd' <> long "destination" <> metavar "PATH" <> help "Path to new project")
      pure Parameters{..}

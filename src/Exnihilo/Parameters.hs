{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Exnihilo.Parameters where

import           Control.Monad.Except
import           Options.Applicative

data Parameters = Parameters
  { paramVariableFile :: FilePath
  , paramTemplateFile :: FilePath
  , paramSaveLocation :: FilePath
  }

getParams :: (MonadIO m) => m Parameters
getParams = liftIO $ execParser $ info (helper <*> opts) fullDesc
  where
    opts = do
      paramVariableFile <- strOption (long "vars"     <> metavar "FILE" <> help "")
      paramTemplateFile <- strOption (long "template" <> metavar "FILE" <> help "")
      paramSaveLocation <- strOption (long "output"   <> metavar "FILE" <> help "")
      pure Parameters{..}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad.Except
import           Data.Version         (showVersion)
import           System.Environment
import           System.Exit

import           Exnihilo.App
import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Schema
import           Exnihilo.Variables
import           Paths_exnihilo

run :: (MonadIO m) => Parameters -> Variables -> m ()
run params vars = runApp params vars $ do
  rawSchema <- getRawSchema
  templateSchema <- parseRawSchema rawSchema
  tryGetMissingVariables templateSchema
  renderedSchema <- renderTemplateSchema templateSchema
  saveRenderedSchema renderedSchema

runInit :: (MonadIO m) => m (Either Error (Parameters, Variables))
runInit = runExceptT $ do
  mode <- getParams
  case mode of
    ModeVersion params -> do
      if paramVersionNumeric params
        then liftIO $ putStrLn $ showVersion version
        else liftIO $ putStrLn $ ("exnihilo v" <>)  $ showVersion version
      liftIO exitSuccess
    ModeCreate params@Parameters{..} -> do
      vars <- safeDecodeYamlFile paramVariableFile
      pure (params, vars)

main :: IO ()
main = do
  res <- runInit
  case res of
    Left e               -> printError e
    Right (params, vars) -> run params vars

debug :: String -> IO ()
debug = flip withArgs main . words

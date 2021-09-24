{-# LANGUAGE RecordWildCards #-}
module Exnihilo.Main where

import           Control.Monad.Except
import           Control.Monad.Reader
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

run :: App ()
run = do
  Parameters{..} <- ask

  unless paramNoImplicitVariables addImplicitVariables
  applyOverrides paramVariableOverrides

  rawSchema <- getRawSchema
  templateSchema <- parseRawSchema rawSchema
  handleMissingVariables templateSchema
  typeCheckTemplateSchema templateSchema
  renderedSchema <- renderTemplateSchema templateSchema
  unless paramDryRun $ saveRenderedSchema renderedSchema

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
      vars <- case paramVariableFile of
        Just paramVariableFile' -> safeDecodeYamlFile paramVariableFile'
        Nothing                 -> pure mempty
      pure (params, vars)

main :: IO ()
main = do
  res <- runInit
  case res of
    Left e               -> printError e
    Right (params, vars) -> runApp params vars run

debug :: String -> IO ()
debug = flip withArgs main . words

mainTry :: IO (Either Error ())
mainTry = do
  res <- runInit
  case res of
    Left e               -> pure $ Left e
    Right (params, vars) -> tryRunApp params vars run

debugTry :: String -> IO (Either Error ())
debugTry = flip withArgs mainTry . words

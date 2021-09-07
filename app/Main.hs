module Main where

import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Data.Version             (showVersion)
import           System.Environment
import           System.Exit

import           Exnihilo.App
import           Exnihilo.Env
import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Schema
import           Paths_exnihilo

run :: (MonadIO m) => Env -> m ()
run env = runApp env $ do
  asks (paramTemplateFile . envCliParams)
  >>= safeDecodeYamlFile
  >>= parseTemplateSchema
  >>= renderTemplateSchema
  >>= saveRenderedSchema

runInit :: (MonadIO m) => m (Either Error Env)
runInit = runExceptT $ do
  mode <- getParams
  case mode of
    ModeVersion params -> do
      if paramVersionNumeric params
        then liftIO $ putStrLn $ showVersion version
        else liftIO $ putStrLn $ ("exnihilo v" <>)  $ showVersion version
      liftIO exitSuccess
    ModeCreate params -> do
      vars   <- safeDecodeYamlFile $ paramVariableFile params
      pure $ Env vars params

main :: IO ()
main = do
  res <- runInit
  case res of
    Left e    -> printError e
    Right env -> run env

debug :: String -> IO ()
debug = flip withArgs main . words

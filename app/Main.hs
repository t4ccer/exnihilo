module Main where

import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           System.Environment

import           Exnihilo.App
import           Exnihilo.Env
import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.SafeIO
import           Exnihilo.Schema

run :: (MonadIO m) => Env -> m ()
run env = runApp env $
      asks (paramTemplateFile . envCliParams)
  >>= safeDecodeYamlFile
  >>= parseTemplateSchema
  >>= renderTemplateSchema
  >>= saveRenderedSchema

runInit :: (MonadIO m) => m (Either Error Env)
runInit = runExceptT $ do
  params <- getParams
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

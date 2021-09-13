{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Exnihilo.Hook where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Foldable        (traverse_)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Yaml
import           System.Command       hiding (cmd)
import           System.Exit

import           Exnihilo.Error
import           Exnihilo.Parameters
import           Exnihilo.Variables

data Hook a
  = Hook
  { hookName    :: Maybe Text
  , hookCommand :: a
  , hookBind    :: Maybe Text
  }
  deriving (Show, Functor)

instance FromJSON (Hook Text) where
  parseJSON = withObject "Hook" $ \v -> do
    hookName    <- v .:? "name"
    hookCommand <- v .:  "run"
    hookBind    <- v .:? "bind"
    pure Hook{..}

runHooks :: (MonadError Error m, MonadIO m, MonadState Variables m, MonadReader Parameters m) => [Hook Text] -> m ()
runHooks = traverse_ runHook

-- TODO validate hook command
runHook :: (MonadError Error m, MonadIO m, MonadState Variables m, MonadReader Parameters m) => Hook Text -> m ()
runHook Hook{..} = do
  case words $ T.unpack hookCommand of
    (cmd:args) -> do
      Parameters{..} <- ask
      (Exit c, Stdout out, Stderr err) <- liftIO $ command [Cwd paramSaveLocation] cmd args
      case c of
        ExitSuccess   -> case hookBind of
          Nothing -> pure ()
          Just k  -> parseVariable k (T.pack out) >>= applyOverrides
        ExitFailure _ -> throwError $ ErrorCommandExitCode $ T.pack err -- TODO add error type
    _ -> throwError $ ErrorInvalidCommand hookCommand

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Exnihilo.Error where

import           Control.Monad.Except
import           Data.Text            (Text)
import qualified Data.Text.IO         as T
import           System.IO

data Error
  = ErrorMissingVariable Text
  | ErrorFileRead Text
  | ErrorFileWrite Text
  | ErrorOther Text
  deriving (Show)

prettyError :: Error -> Text
prettyError = \case
  ErrorFileRead e        -> mconcat ["[Error] File read error. ", e]
  ErrorMissingVariable e -> mconcat ["[Error] Missing variable in schema. ", e]
  ErrorFileWrite e       -> mconcat ["[Error] File write error. ", e]
  ErrorOther e           -> mconcat ["[Error] Unknown error occured. ", e]

printError :: MonadIO m => Error -> m ()
printError = liftIO . T.hPutStrLn stderr . prettyError

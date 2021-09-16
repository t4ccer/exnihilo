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
  | ErrorFileReadMissing Text
  | ErrorFileWrite Text
  | ErrorVariableParse Text
  | ErrorFileParse Text Text
  | ErrorTemplateParse Text
  | ErrorUrlFetch Text
  | ErrorUrlInvalid Text
  | ErrorCreateTypeCheck Text
  | ErrorCommandExitCode Text
  | ErrorInvalidCommand Text
  | ErrorOther Text
  deriving (Show, Eq)

-- TODO add some string interpolation lib
prettyError :: Error -> Text
prettyError = \case
  ErrorFileRead e        -> mconcat ["[Error] File read error. ", e]
  ErrorFileReadMissing e -> mconcat ["[Error] File read error. File '", e, "' does not exist."]
  ErrorMissingVariable e -> mconcat ["[Error] Variable used in schema but not defined: ", e]
  ErrorFileWrite e       -> mconcat ["[Error] File write error. ", e]
  ErrorOther e           -> mconcat ["[Error] Unknown error occured: ", e]
  ErrorTemplateParse e   -> mconcat ["[Error] Could not parse template. Report this to schema author. ", e]
  ErrorUrlFetch e        -> mconcat ["[Error] Could not fetch schema from url: \"", e, "\""]
  ErrorCreateTypeCheck e -> mconcat ["[Error] Create condition variable must be bool ('", e, "')"]
  ErrorUrlInvalid e      -> mconcat ["[Error] Invalid url: \"", e, "\""]
  ErrorCommandExitCode e -> mconcat ["[Error] Hook exited with non-zero exit code: \"", e, "\""]
  ErrorInvalidCommand e  -> mconcat ["[Error] Invalid hook command: \"", e, "\""]
  ErrorFileParse fp e    -> mconcat ["[Error] Could not parse file '", fp, "': ", e]
  ErrorVariableParse e   -> mconcat ["[Error] Could not parse variable '", e, "'"]

printError :: MonadIO m => Error -> m ()
printError = liftIO . T.hPutStrLn stderr . prettyError

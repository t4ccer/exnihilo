{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exnihilo.Template where

import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Exnihilo.Error
import           Exnihilo.Variables

data Template
  = TemplateVariable Text
  | TemplateConstant Text
  deriving (Show)

-- TODO add variable support
parseTemplate :: Monad m => Text -> m [Template]
parseTemplate = pure . pure . TemplateConstant

renderTemplate :: forall m r. (MonadReader r m, Has Variables r, MonadError Error m) => [Template] -> m Text
renderTemplate templ = do
  vars :: Variables <- ask
  let
    go :: Template -> m Text
    go = \case
       (TemplateVariable var) -> case lookupVariable vars var of
         Nothing -> throwError $ ErrorMissingVariable var
         Just v  -> pure v
       (TemplateConstant cons) -> pure cons
  T.concat <$> traverse go templ

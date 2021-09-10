{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Exnihilo.Template where

import           Control.Monad.Except
import           Control.Monad.Reader.Has
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Exnihilo.Error
import           Exnihilo.Variables

type Parser a = Parsec Void Text a

-- TODO add AST, etc
data Template
  = TemplateVariable Text
  | TemplateConstant Text
  deriving (Show)

parseTemplate :: MonadError Error m => Text -> m [Template]
parseTemplate = maybe (throwError $ ErrorTemplateParse "") pure . parseMaybe templateP

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

templateP :: Parser [Template]
templateP = fmap concat $ many $ choice [try varP, try textP, try oneBraceP]

varP :: Parser [Template]
varP = string "{{" *> (pure . TemplateVariable . T.strip . T.pack <$> many (anySingleBut '}')) <* wsP <* string "}}"

wsP :: Parser String
wsP = many (char ' ')

textP :: Parser [Template]
textP = pure . TemplateConstant . T.pack <$> some (anySingleBut '{')

oneBraceP :: Parser [Template]
oneBraceP = pure . TemplateConstant . T.singleton <$> char '{'

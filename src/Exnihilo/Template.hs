{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
-- {-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module Exnihilo.Template where

import           Control.Monad.Except
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Control.Monad.State
import           Exnihilo.Error
import           Exnihilo.Variables

type Parser a = Parsec Void Text a

newtype Template = Template {getTemplateAst :: [TemplateAst]}
  deriving (Semigroup)

instance Monoid Template where
  mempty = Template $ pure $ TemplateConstant ""

-- TODO add AST, etc
data TemplateAst
  = TemplateVariable Text
  | TemplateConstant Text
  deriving (Show, Eq)

astVariable :: TemplateAst -> Maybe Text
astVariable (TemplateVariable name) = pure name
astVariable _                       = Nothing

requiredVariables :: Template -> [Text]
requiredVariables = mapMaybe astVariable . getTemplateAst

parseTemplate :: MonadError Error m => Text -> m Template
parseTemplate = maybe (throwError $ ErrorTemplateParse "") pure . parseMaybe templateP

renderTemplate :: forall m. (MonadState Variables m, MonadError Error m) => Template -> m Text
renderTemplate templ = do
  vars <- get
  let
    go :: TemplateAst -> m Text
    go = \case
       (TemplateVariable var) -> case lookupVariable vars var of
         Nothing -> throwError $ ErrorMissingVariable var
         Just v  -> pure v
       (TemplateConstant cons) -> pure cons
  fmap T.concat $ traverse go $ getTemplateAst templ

templateP :: Parser Template
templateP = fmap (Template . concat) $ many $ choice [try varP, try textP, try oneBraceP]

varP :: Parser [TemplateAst]
varP = string "{{" *> (pure . TemplateVariable . T.strip . T.pack <$> many (anySingleBut '}')) <* wsP <* string "}}"

wsP :: Parser String
wsP = many (char ' ')

textP :: Parser [TemplateAst]
textP = pure . TemplateConstant . T.pack <$> some (anySingleBut '{')

oneBraceP :: Parser [TemplateAst]
oneBraceP = pure . TemplateConstant . T.singleton <$> char '{'

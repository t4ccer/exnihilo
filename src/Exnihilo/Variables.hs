{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Exnihilo.Variables where

import           Data.Aeson.Types
import           Data.Bifunctor
import qualified Data.HashMap.Strict as HM
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Text           (Text)
import qualified Data.Text           as T

newtype Variables = Variables {getVariables :: Map Text Text}
  deriving (Semigroup, Monoid)

instance FromJSON Variables where
  parseJSON = withObject "Variables" $ \obj -> do
    let yamlDict = HM.toList obj
        strLst = map (second getText) yamlDict
        wrongEntry = bimap T.unpack getType $ head $ filter (not . isText . snd) yamlDict
    if all (isText . snd) yamlDict
      then pure $ Variables $ M.fromList strLst
      else parseFail ("Variable can be only number or string. Variable \"" <> fst wrongEntry <> "\" is type of " <> snd wrongEntry <> ".")
    where
      isText (String _) = True
      isText (Number _) = True
      isText _          = False
      getText (String s) = s
      getText (Number n) = T.pack $ show n
      getText _          = undefined -- Safe when used with check
      getType (String _) = "string"
      getType (Object _) = "object"
      getType (Array _)  = "array"
      getType (Number _) = "number"
      getType (Bool _)   = "bool"
      getType Null       = "null"

fromList :: [(Text, Text)] -> Variables
fromList = Variables . M.fromList

lookupVariable :: Variables -> Text -> Maybe Text
lookupVariable (Variables vars) key = M.lookup key vars


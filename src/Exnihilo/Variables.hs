{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Exnihilo.Variables where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Aeson.Types
import           Data.Bifunctor
import qualified Data.HashMap.Strict  as HM
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Scientific
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           GHC.IO.Handle        (hFlush)
import           System.IO            (stdout)

import           Data.Char            (toLower)
import           Exnihilo.Error
import           Exnihilo.SafeIO

data Variable
  = VarString Text
  | VarNumber Scientific
  | VarBool   Bool
  deriving (Show)

newtype Variables = Variables {getVariables :: Map Text Variable}
  deriving (Show, Semigroup, Monoid)

instance FromJSON Variables where
  parseJSON = withObject "Variables" $ \obj -> do
    let yamlDict = HM.toList obj
        strLst = map (second getText) yamlDict
        wrongEntry = bimap T.unpack getType $ head $ filter (not . isText . snd) yamlDict
    if all (isText . snd) yamlDict
      then pure $ Variables $ M.fromList strLst
      else parseFail ("Variable can be only number,string, or bool. Variable \"" <> fst wrongEntry <> "\" is type of " <> snd wrongEntry <> ".")
    where
      isText (String _) = True
      isText (Number _) = True
      isText (Bool   _) = True
      isText _          = False
      getText (String s) = VarString s
      getText (Number n) = VarNumber n
      getText (Bool b)   = VarBool b
      getText _          = undefined -- Safe when used with check
      getType (String _) = "string"
      getType (Object _) = "object"
      getType (Array _)  = "array"
      getType (Number _) = "number"
      getType (Bool _)   = "bool"
      getType Null       = "null"

fromList :: [(Text, Variable)] -> Variables
fromList = Variables . M.fromList

lookupVariable :: Variables -> Text -> Maybe Variable
lookupVariable (Variables vars) key = M.lookup key vars

parseVariable :: (MonadError Error m) => Text -> Text -> m Variables
parseVariable k v = safeDecodeYaml $ k  <>  ": " <> v

askForVariable :: (MonadIO m, MonadState Variables m, MonadError Error m) => Text -> m ()
askForVariable missing =  do
  value <- liftIO $ do
    T.putStrLn $ "Variable '" <> missing <> "' not defined in variables file. Provide value for missing variable."
    T.putStr $ missing <> ": "
    hFlush stdout
    T.getLine
  (Variables var1) <- parseVariable missing value
  (Variables var2) <- get
  put $ Variables $ M.union var1 var2

askForMissingVariables :: forall m. (MonadIO m, MonadState Variables m, MonadError Error m) => [Text] -> m ()
askForMissingVariables missing = do
  new <- execStateT (go missing) mempty
  applyOverrides new
  where
    go []     = pure ()
    go (x:xs) = askForVariable x >> go xs

applyOverrides :: (MonadState Variables m) => Variables -> m ()
applyOverrides vars =
  modify (Variables . M.union (getVariables vars) . getVariables)

renderVariable :: Variable -> Text
renderVariable (VarString s) = s
renderVariable (VarNumber n) = T.pack $ show n
renderVariable (VarBool n)   = T.pack $ map toLower $ show n

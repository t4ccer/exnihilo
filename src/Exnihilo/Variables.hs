{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Exnihilo.Variables where

import           Control.Monad.Except
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Exnihilo.Error
import           Exnihilo.SafeIO

newtype Variables = Variables {getVariables :: Map Text Text}

lookupVariable :: Variables -> Text -> Maybe Text
lookupVariable (Variables vars) key = M.lookup key vars

-- TODO make it efficient, add errors, switch to yaml
varsFromLine :: forall m. Monad m => Text -> m (Text, Text)
varsFromLine = go mempty
  where
    go :: Text -> Text -> m (Text, Text)
    go acc inp = if T.head inp == '='
      then pure (acc, T.tail inp)
      else go (acc <> T.singleton (T.head inp)) (T.tail inp)

varsFromFile :: (MonadIO m, MonadError Error m) => FilePath -> m Variables
varsFromFile fp = do
  content <- safeReadFile fp
  vars <- mapM varsFromLine $ T.lines content
  pure $ Variables $ M.fromList vars

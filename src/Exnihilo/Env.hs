{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Exnihilo.Env where

import           Control.Monad.Reader.Has
import           GHC.Generics

import           Exnihilo.Parameters
import           Exnihilo.Variables

data  Env = Env
  { envVariables :: Variables
  , envCliParams :: Parameters
  }
  deriving (Generic, Has Variables, Has Parameters)

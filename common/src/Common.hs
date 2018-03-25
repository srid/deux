{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Common where

import GHC.Generics
import Data.Aeson

import Dhall

data Task = Task
  { text :: Text
  , done :: Bool
  }
  deriving (Generic, Show)

instance Interpret Task
instance ToJSON Task
instance FromJSON Task
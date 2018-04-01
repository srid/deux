{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Task where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import GHC.Generics

import Dhall

data Task = Task
  { _taskTitle :: Text
  , _taskDone :: Bool
  , _taskContext :: [Text]
  , _taskDescription :: Text
  }
  deriving (Generic, Show)

instance Interpret Task
instance Inject Task
instance ToJSON Task
instance FromJSON Task

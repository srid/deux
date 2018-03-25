{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common (Task, DemoAPI) where

import GHC.Generics
import Data.Aeson

import Dhall
import Servant.API

data Task = Task
  { text :: Text
  , done :: Bool
  }
  deriving (Generic, Show)

instance Interpret Task
instance ToJSON Task
instance FromJSON Task

type DemoAPI = "demo" :> Get '[JSON] [Task]

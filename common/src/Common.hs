{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common where

import GHC.Generics
import Data.Aeson
import Data.Default

import Dhall
import Servant.API

data Task = Task
  { _taskTitle :: Text
  , _taskDone :: Bool
  , _taskContext :: [Text]
  , _taskDescription :: Text
  }
  deriving (Generic, Show)

data Piece = Piece
  { _pieceTitle :: Text
  , _pieceBody :: Text
  }
  deriving (Generic, Show)

data Demo = Demo
  { _demoTasks :: [Task]
  , _demoPieces :: [Piece]
  }
  deriving (Generic, Show)

instance Default Demo where
  def = Demo def def

instance Interpret Task
instance ToJSON Task
instance FromJSON Task

instance Interpret Piece
instance ToJSON Piece
instance FromJSON Piece

instance Interpret Demo
instance ToJSON Demo
instance FromJSON Demo

type DemoAPI = "demo" :> Get '[JSON] Demo

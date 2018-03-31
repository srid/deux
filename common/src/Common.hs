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

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isUpper)
import Data.Default
import Data.Scientific
import Data.Text.Lazy as TL
import GHC.Generics

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

data CostcoTransaction = CostcoTransaction
  { _costcotransactionTransactionDate :: Text
  , _costcotransactionDescription :: Text
  , _costcotransactionCategory :: Text
  , _costcotransactionDebit :: Maybe Scientific
  , _costcotransactionCredit :: Maybe Scientific
  }

data Demo = Demo
  { _demoTasks :: [Task]
  , _demoPieces :: [Piece]
  }
  deriving (Generic, Show)

instance Default Demo where
  def = Demo def def

interpretOptions :: InterpretOptions
interpretOptions = defaultInterpretOptions { fieldModifier = f }
  where
    f = TL.toLower . TL.dropWhile (not . isUpper)

instance Interpret Task
instance Inject Task
instance ToJSON Task
instance FromJSON Task

instance Interpret Piece
instance Inject Piece
instance ToJSON Piece
instance FromJSON Piece

instance Interpret Demo
instance Inject Demo
instance ToJSON Demo
instance FromJSON Demo

type DemoAPI = "demo" :> Get '[JSON] Demo

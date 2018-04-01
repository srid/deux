{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Finance where

import Data.Aeson (FromJSON, ToJSON)
import Data.Scientific
import Data.Text.Lazy as TL
import GHC.Generics

import Dhall

data CostcoTransaction = CostcoTransaction
  { _costcotransactionTransactionDate :: Text
  , _costcotransactionDescription :: Text
  , _costcotransactionCategory :: Text
  , _costcotransactionAmount :: Amount
  }
  deriving (Generic, Show)

data Amount
  = Debit Scientific
  | Credit Scientific
  deriving (Generic, Show)

instance Interpret CostcoTransaction
instance Inject CostcoTransaction
instance ToJSON CostcoTransaction
instance FromJSON CostcoTransaction

instance Interpret Amount
instance Inject Amount
instance ToJSON Amount
instance FromJSON Amount

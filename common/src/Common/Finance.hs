{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  deriving (Generic, Show, Interpret, Inject, ToJSON, FromJSON)

data Amount
  = Debit Scientific
  | Credit Scientific
  deriving (Generic, Show, Interpret, Inject, ToJSON, FromJSON)

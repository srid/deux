{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common.Finance where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Data.Scientific
import Data.Text.Lazy as TL
import GHC.Generics

import Dhall
import Data.Sv
import qualified Data.Sv.Decode as D

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

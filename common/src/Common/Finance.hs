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
  , _costcotransactionDebit :: Maybe Double
  , _costcotransactionCredit :: Maybe Double
  }
  deriving (Generic, Show)

instance Interpret CostcoTransaction
instance Inject CostcoTransaction
instance ToJSON CostcoTransaction
instance FromJSON CostcoTransaction

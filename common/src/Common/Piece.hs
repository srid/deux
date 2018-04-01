{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Piece where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import GHC.Generics

import Dhall

data Piece = Piece
  { _pieceTitle :: Text
  , _pieceBody :: Text
  }
  deriving (Generic, Show)

instance Interpret Piece
instance Inject Piece
instance ToJSON Piece
instance FromJSON Piece

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Common.Piece where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text.Lazy (Text)
import GHC.Generics

import Dhall

data Piece = Piece
  { _pieceTitle :: Text
  , _pieceBody :: Text
  }
  deriving (Generic, Show, Interpret, Inject, ToJSON, FromJSON)

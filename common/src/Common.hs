{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common
  ( Demo(..)
  , DemoAPI
  , interpretOptions
  , module Common.Piece
  , module Common.Task
  )where

import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isUpper)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import GHC.Generics

import Dhall
import Servant.API

import Common.Piece
import Common.Task

-- TODO: Rename Demo to something meaningful
data Demo = Demo
  { _demoTasks :: [Task]
  , _demoPieces :: [Piece]
  }
  deriving (Generic, Show)

type DemoAPI = "demo" :> Get '[JSON] (Either Text Demo)

-- Follow record field naming conventions in this project when converting back
-- and forth from Dhall fields.
interpretOptions :: InterpretOptions
interpretOptions = defaultInterpretOptions { fieldModifier = f }
  where
    f = TL.toLower . TL.dropWhile (not . isUpper)

instance Interpret Demo
instance Inject Demo
instance ToJSON Demo
instance FromJSON Demo

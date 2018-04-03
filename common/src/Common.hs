{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Common
  ( Env(..)
  , Demo(..)
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

data Env = Env
  { _envDhallDataDir :: !Text
  , _envDemoFile :: !FilePath
  }

-- TODO: Rename Demo to something meaningful
data Demo = Demo
  { _demoTasks :: [Task]
  , _demoPieces :: [Piece]
  }
  deriving (Generic, Show, Interpret, Inject, ToJSON, FromJSON)

type DemoAPI = "demo" :> Get '[JSON] (Either Text Demo)

-- Follow record field naming conventions in this project when converting back
-- and forth from Dhall fields.
interpretOptions :: InterpretOptions
interpretOptions = defaultInterpretOptions { fieldModifier = f }
  where
    f "_1" = "_1"  -- Keep Dhall's sum type record fields as is.
    f x = TL.toLower . TL.dropWhile (not . isUpper) $ x

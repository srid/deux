{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T
import Control.Monad.IO.Class (liftIO)

import Dhall

import Web.Scotty

import Common

main = scotty 3001 $
  get "/" $ do
    -- TODO: use 3 layer cake
    x :: Task <- liftIO $ input auto "../demo.dhall"
    html $ T.pack $ show x

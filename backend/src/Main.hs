{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T

import Dhall

import Servant
import Web.Scotty

import Network.Wai
import Network.Wai.Handler.Warp

import Common

type DemoAPI = "demo" :> Get '[JSON] Task

server :: Server DemoAPI
server = do
  x :: Task <- liftIO $ input auto "../demo.dhall"
  return x

demoAPI :: Proxy DemoAPI
demoAPI = Proxy

app :: Application
app = serve demoAPI server

main :: IO ()
main = do
  putStrLn "Running backend at http://localhost:3001"
  run 3001 app

-- IDEAS:
-- Abstract a backend/frontend to get a single JSON.
  -- pull fro slownews getpostbuild/json stuff

oldMain = scotty 3001 $
  get "/" $ do
    -- TODO: use 3 layer cake
    x :: Task <- liftIO $ input auto "../demo.dhall"
    html $ T.pack $ show x

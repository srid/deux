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
import Data.Monoid ((<>))

import Dhall
import Dhall.Core (pretty)

import Servant

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

baseDir :: Text
baseDir = "/home/srid/Dropbox/deuxContent/"

readDhallFile :: Interpret a => Text -> IO a
readDhallFile path = input (autoWith interpretOptions) $ baseDir <> path

dumpDhall :: Inject a => a -> Text
dumpDhall = pretty . embed (injectWith interpretOptions)

server :: Server DemoAPI
server = do
  liftIO parseDemo

parseDemo :: IO Demo
parseDemo = do
  tasks :: [Task] <- readDhallFile "Inbox.dhall"
  pieces :: [Piece] <- readDhallFile "Piece.dhall"
  return $ Demo tasks pieces

demoAPI :: Proxy DemoAPI
demoAPI = Proxy

app :: Application
app = serve demoAPI server

main :: IO ()
main = do
  _ <- parseDemo
  run 3001 $ simpleCors $ logStdoutDev $ app

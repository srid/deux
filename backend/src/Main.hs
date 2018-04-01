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

import Data.Bifunctor (first)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Monoid ((<>))

import Data.Text.Lazy as T
import Dhall
import Dhall.Core (pretty)
import Dhall.Parser (ParseError)

import Servant

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common
import Common.Finance

import Backend

server :: Server DemoAPI
server = do
  liftIO $ do
    r :: Either SomeException Demo <- try parseDemo
    return $ first (T.pack . show) r

parseDemo :: IO Demo
parseDemo = do
  putStrLn "Reading dhall files"
  tasks :: [Task] <- readDhallFile "Inbox.dhall"
  pieces :: [Piece] <- readDhallFile "Piece.dhall"
  return $ Demo tasks pieces

demoAPI :: Proxy DemoAPI
demoAPI = Proxy

app :: Application
app = serve demoAPI server

main :: IO ()
main = do
  _ :: Either SomeException Demo <- try parseDemo
  putStrLn "Running http://localhost:3001/"
  run 3001 $ simpleCors $ logStdoutDev $ app

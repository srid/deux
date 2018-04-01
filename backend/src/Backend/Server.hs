{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server where

import Data.Bifunctor (first)
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)

import Data.Text.Lazy as T

import Servant

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

import Backend

server :: Server DemoAPI
server = do
  liftIO $ do
    r :: Either SomeException Demo <- try parseDemo
    return $ first (T.pack . show) r

demoAPI :: Proxy DemoAPI
demoAPI = Proxy

app :: Application
app = serve demoAPI server

runServer :: IO ()
runServer = do
  putStrLn "Running server at http://localhost:3001/"
  run 3001 $ simpleCors $ logStdoutDev $ app

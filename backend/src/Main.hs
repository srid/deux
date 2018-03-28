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

import Data.Monoid ((<>))
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy as TL
import Data.Char (isUpper)

import Dhall

import Servant

import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

baseDir :: Text
baseDir = "/home/srid/Dropbox/deuxContent/"

readDhallFile :: Interpret a => Text -> IO a
readDhallFile path = input interpretOptions $ baseDir <> path
  where
    interpretOptions = autoWith
      (defaultInterpretOptions { fieldModifier = TL.toLower . TL.dropWhile (not . isUpper) })

server :: Server DemoAPI
server = do
  tasks :: [Task] <- liftIO $ readDhallFile "Inbox.dhall"
  pieces :: [Piece] <- liftIO $ readDhallFile "Piece.dhall"
  return $ Demo tasks pieces

demoAPI :: Proxy DemoAPI
demoAPI = Proxy

app :: Application
app = serve demoAPI server

main :: IO ()
main = do
  run 3001 $ simpleCors $ logStdoutDev $ app

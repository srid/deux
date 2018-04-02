{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Exception (try, SomeException)
import Control.Concurrent.Async.Lifted.Safe
import Control.Monad.Reader
import Control.Concurrent.STM

import Common
import Backend
import qualified Backend.Server as Server

app :: (Functor m, MonadReader Env m, MonadIO m) => m ()
app = do
  _ :: Either SomeException Demo <- liftIO $ try parseDemo'
  liftIO $ Server.runServer

main :: IO ()
main = do
  -- TODO: Read from configuration file
  let env = Env
        "/home/srid/Dropbox/Documents/"
        "/home/srid/Dropbox/Documents/2018/BankStatements/CapitalOne/Stmnt_022018_4997.csv"
  runReaderT app env

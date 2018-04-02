{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Reader
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Validation as V

import Backend
import qualified Backend.Server as Server
import Common

app :: (Functor m, MonadReader Env m, MonadIO m) => m ()
app = do
  _ <- parseDemo
  Server.runServer

main :: IO ()
main = main' True

main' :: Bool -> IO ()
main' shouldServe = do
  -- TODO: Read from configuration file
  let env = Env
        "/home/srid/Dropbox/Documents/"
        "/home/srid/Dropbox/Documents/2018/BankStatements/CapitalOne/Stmnt_022018_4997.csv"
  case shouldServe of
    True -> runReaderT app env
    False -> runReaderT dumpTmp env

dumpTmp :: (Functor m, MonadReader Env m, MonadIO m) => m ()
dumpTmp = do
  txs' <- transactions
  let (Right txs) = V.toEither txs'
  liftIO $ TIO.writeFile "/tmp/txs.dhall" $ dumpDhall txs


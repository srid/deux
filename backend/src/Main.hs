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

import Common (Env(Env))

import Backend
import qualified Backend.Server as Server
import Backend.Finance (dumpTmp)

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
  -- TODO: Replace this ugliness with cli arg parser
  case shouldServe of
    True -> runReaderT app env
    False -> runReaderT dumpTmp env

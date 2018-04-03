{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend where

import Control.Monad.Reader
import Data.Monoid ((<>))

import Dhall
import Dhall.Core (pretty)

import Common

readDhallFile
  :: (Functor m, MonadReader Env m, MonadIO m, Interpret a)
  => Text -> m a
readDhallFile path = do
  baseDir <- reader _envDhallDataDir
  liftIO $ input (autoWith interpretOptions) $ baseDir <> path

dumpDhall :: Inject a => a -> Text
dumpDhall = pretty . embed (injectWith interpretOptions)

parseDemo
  :: (Functor m, MonadReader Env m, MonadIO m)
  => m Demo
parseDemo = do
  liftIO $ putStrLn "Reading dhall files..."
  tasks :: [Task] <- readDhallFile "Inbox.dhall"
  pieces :: [Piece] <- readDhallFile "Piece.dhall"
  return $ Demo tasks pieces

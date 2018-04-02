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
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Sv
import qualified Data.Sv.Decode as D

import Dhall
import Dhall.Core (pretty)

import Common
import Common.Finance

data Env = Env
  { envDhallDataDir :: !Text
  , envDemoFile :: !FilePath
  }

readDhallFile
  :: (Functor m, MonadReader Env m, MonadIO m, Interpret a)
  => Text -> m a
readDhallFile path = do
  e :: Env <- ask
  let p = (envDhallDataDir e) <> path
  liftIO $ input (autoWith interpretOptions) p

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
  -- liftIO $ parseDemo'
costcoTransactionDecoder :: Decode' ByteString CostcoTransaction
costcoTransactionDecoder =
  CostcoTransaction
    <$> (D.ignore *> s)
    <*> (D.ignore *> D.ignore *> s)
    <*> s
    <*> (Debit . read <$> (D.string <* D.emptyField) <!>
         Credit . read <$> (D.emptyField *> D.string))
  where
    s = D.lazyUtf8

transactions
  :: (Functor m, MonadReader Env m, MonadIO m)
  => m (DecodeValidation ByteString [CostcoTransaction])
transactions = do
  e <- ask
  parseDecodeFromFile'
    attoparsecByteString
    costcoTransactionDecoder
    defaultParseOptions
    (envDemoFile e)

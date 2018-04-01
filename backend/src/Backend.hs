{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Scientific
import Data.Sv
import qualified Data.Sv.Decode as D
import Data.Text.Lazy.IO as TIO

import qualified Data.Validation as V
import Dhall
import Dhall.Core (pretty)

import Common
import Common.Finance

baseDir :: Text
baseDir = "/home/srid/Dropbox/deuxContent/"

readDhallFile :: Interpret a => Text -> IO a
readDhallFile path = input (autoWith interpretOptions) $ baseDir <> path

dumpDhall :: Inject a => a -> Text
dumpDhall = pretty . embed (injectWith interpretOptions)

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

transactions :: IO (DecodeValidation ByteString [CostcoTransaction])
transactions = parseDecodeFromFile' attoparsecByteString costcoTransactionDecoder defaultParseOptions demoFile

dumpTmp :: IO ()
dumpTmp = do
  txs' <- transactions
  let (Right txs) = V.toEither txs'
  TIO.writeFile "/tmp/txs.dhall" $ dumpDhall txs

demoFile :: FilePath
demoFile = "/home/srid/Dropbox/Documents/2018/BankStatements/CapitalOne/Stmnt_022018_4997.csv"

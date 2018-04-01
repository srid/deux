{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend where

import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Text.Lazy.IO as TIO

import qualified Data.Validation as V
import Dhall
import Dhall.Core (pretty)

import Common
import Common.Finance

-- TODO: Read this from config file.
-- Perhaps use this opportunity to use 'three layer cake' from now
baseDir :: Text
baseDir = "/home/srid/Dropbox/deuxContent/"

demoFile :: FilePath
demoFile = "/home/srid/Dropbox/Documents/2018/BankStatements/CapitalOne/Stmnt_022018_4997.csv"

readDhallFile :: Interpret a => Text -> IO a
readDhallFile path = input (autoWith interpretOptions) $ baseDir <> path

dumpDhall :: Inject a => a -> Text
dumpDhall = pretty . embed (injectWith interpretOptions)

parseDemo :: IO Demo
parseDemo = do
  putStrLn "Reading dhall files"
  tasks :: [Task] <- readDhallFile "Inbox.dhall"
  pieces :: [Piece] <- readDhallFile "Piece.dhall"
  return $ Demo tasks pieces

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


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.Finance where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy.IO as TIO

import qualified Data.Validation as V
import Data.Sv
import qualified Data.Sv.Decode as D

import Common (Env(..))
import Common.Finance

import Backend (dumpDhall)

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
  txsFile <- reader _envDemoFile
  parseDecodeFromFile'
    attoparsecByteString
    costcoTransactionDecoder
    defaultParseOptions
    txsFile

-- XXX: Scratch
dumpTmp :: (Functor m, MonadReader Env m, MonadIO m) => m ()
dumpTmp = do
  txs' <- transactions
  let (Right txs) = V.toEither txs'
  liftIO $ TIO.writeFile "/tmp/txs.dhall" $ dumpDhall txs
  

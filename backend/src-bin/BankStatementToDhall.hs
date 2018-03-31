{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Common.Finance

import Data.ByteString (ByteString)
import Data.Sv
import qualified Data.Sv.Decode as D

costcoTransactionDecoder :: Decode' ByteString CostcoTransaction
costcoTransactionDecoder =
  mk
    <$> D.ignore -- stage
    <*> s
    <*> D.ignore -- posted data
    <*> D.ignore -- card no
    <*> s
    <*> s
    <*> D.orEmpty D.double -- TODO: Use Either here.
    <*> D.orEmpty D.double
  where
    s = D.lazyUtf8
    -- XXX: remove this ugliness
    mk () a () () b c d e = CostcoTransaction a b c d e

transactions :: IO (DecodeValidation ByteString [CostcoTransaction])
transactions = parseDecodeFromFile' attoparsecByteString costcoTransactionDecoder defaultParseOptions demoFile

demoFile :: FilePath
demoFile = "/home/srid/Dropbox/Documents/2018/BankStatements/CapitalOne/Stmnt_022018_4997.csv"

main :: IO ()
main = do
  putStrLn "importing.."
  d <- transactions
  print d

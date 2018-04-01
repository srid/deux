{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Common.Finance

import Data.Monoid ((<>))
import Data.ByteString (ByteString)
import Data.Sv
import Data.Scientific
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
    <*> D.orEmpty D.string
    <*> D.orEmpty D.string
  where
    s = D.lazyUtf8
    -- XXX: remove this ugliness
    mk () a () () b c d e = CostcoTransaction a b c $ toAmount d e
    toAmount (Just x) Nothing = Debit $ read x
    toAmount Nothing (Just x) = Credit $ read x
    toAmount x y = error $ -- FIXME: How do I combine columns in sv?
      "Invalid combination: " <> show x <> ", " <> show y

transactions :: IO (DecodeValidation ByteString [CostcoTransaction])
transactions = parseDecodeFromFile' attoparsecByteString costcoTransactionDecoder defaultParseOptions demoFile

demoFile :: FilePath
demoFile = "/home/srid/Dropbox/Documents/2018/BankStatements/CapitalOne/Stmnt_022018_4997.csv"

main :: IO ()
main = do
  putStrLn "importing.."
  d <- transactions
  print d

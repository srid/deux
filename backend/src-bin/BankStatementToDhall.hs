{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Validation as V

import Data.Text.Lazy.IO as TIO

import Backend

main :: IO ()
main = do
  txs' <- transactions
  let (Right txs) = V.toEither txs'
  TIO.writeFile "/tmp/txs.dhall" $ dumpDhall txs

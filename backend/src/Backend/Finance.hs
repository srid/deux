{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend.Finance where

import Control.Monad.Reader
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy.IO as TLIO

import Data.Sv
import qualified Data.Sv.Decode as D
import qualified Data.Validation as V

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
  :: (Functor m, MonadReader () m, MonadIO m)
  => m (DecodeValidation ByteString [CostcoTransaction])
transactions = liftIO $ parse <$> BS.getContents
  where
    parse = parseDecode' attoparsecByteString costcoTransactionDecoder defaultParseOptions

-- XXX: Scratch
dumpTmp :: (Functor m, MonadReader () m, MonadIO m) => m ()
dumpTmp = do
  txs' <- transactions
  let (Right txs) = V.toEither txs'
  -- TODO: Run dhall-format programmatically
  liftIO $ TLIO.putStrLn $ dumpDhall txs

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend where

import Control.Monad.Reader
import qualified Data.Text.Lazy as TL
import System.FilePath ((</>))

import Dhall
import Dhall.Core (pretty)

import Common

readDhallFile
  :: (Functor m, MonadReader Env m, MonadIO m, Interpret a)
  => FilePath -> m a
readDhallFile path = do
  baseDir <- reader _envDhallDataDir
  liftIO $ input (autoWith interpretOptions) $
    TL.pack $ baseDir </> path

dumpDhall :: Inject a => a -> Text
dumpDhall = pretty . embed (injectWith interpretOptions)

parseDonnees
  :: (Functor m, MonadReader Env m, MonadIO m)
  => m Donnees
parseDonnees = do
  liftIO $ putStrLn "Reading dhall files..."
  tasks :: [Task] <- readDhallFile "Inbox.dhall"
  pieces :: [Piece] <- readDhallFile "Piece.dhall"
  return $ Donnees tasks pieces

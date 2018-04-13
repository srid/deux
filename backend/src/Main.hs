{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens.Operators ((<&>))
import Control.Monad.Reader
import Data.Semigroup ((<>))
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Options.Applicative

import Common (Env (Env))

import qualified Backend.Finance as Finance
import qualified Backend.Server as Server

data Command
  = Serve
  | ImportCostcoCsv
  deriving (Eq, Show)

main :: IO ()
main = run =<< execParser (p `withInfo` "Deux backend")
  where
    p = subparser
      (  command "serve" (info (pure Serve) (progDesc "Start HTTP server"))
      <> command "costco" (info (pure ImportCostcoCsv) (progDesc "Convert Costco bank statement CSV")))
    withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Command -> IO ()
run = \case
  Serve -> do
    dhallDir <- getHomeDirectory <&> (</> "Dropbox" </> "Documents")
    runReaderT Server.runServer $ Env dhallDir
  ImportCostcoCsv ->
    runReaderT Finance.dumpTmp ()

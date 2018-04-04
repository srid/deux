{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Reader
import System.Console.CmdArgs
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

import Control.Lens.Operators ((<&>))

import Common (Env (Env))

import Backend
import qualified Backend.Finance as Finance
import qualified Backend.Server as Server

data BackendApp
  = BackendApp_Server {}
  | BackendApp_ImportCostcoCsv {}
  deriving (Data,Typeable,Show,Eq)

main :: IO ()
main = do
  cmd <- cmdArgs $ modes
    [ BackendApp_Server
      {} &= help "Run the backend API server" &= auto
    , BackendApp_ImportCostcoCsv
      {} &= help "Convert Costco bank statement to dhall file"
    ]
    &= help "Deux backend" &= program "backend" &= summary "Backend [dev]"

  case cmd of
    BackendApp_Server -> do
      dhallDir <- getHomeDirectory <&> (</> "Dropbox" </> "Documents")
      runReaderT Server.runServer $ Env dhallDir
    BackendApp_ImportCostcoCsv ->
      runReaderT Finance.dumpTmp $ Env "EMPTYXXX" -- TODO: Use separate config

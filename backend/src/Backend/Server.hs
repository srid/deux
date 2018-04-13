{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server (runServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Data.Monoid ((<>))

import Data.Text.Lazy as T

import Servant
import Servant.Server (hoistServer)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

import Backend

type AppM = ReaderT Env Handler

server :: ServerT DonneesAPI AppM
server = getDonnees
  where getDonnees :: AppM (Either Text Donnees)
        getDonnees = do
          _e <- ask
          Right <$> parseDonnees -- TODO: wrap in Either, handle exception. how in readerT?
          -- r :: Either SomeException Donnees <- try parseDonnees
          -- return $ first (T.pack . show) r

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Env -> Application
app s = serve donnesAPI $ hoistServer donnesAPI (nt s) server

donnesAPI :: Proxy DonneesAPI
donnesAPI = Proxy

runServer
  :: (Functor m, MonadReader Env m, MonadIO m)
  => m ()
runServer = do
  e <- ask
  liftIO $ putStrLn $ "Config: " <> show e

  -- Parse once and fail (for ghcid)
  _ <- parseDonnees

  liftIO $ putStrLn "Running server at http://localhost:3001/"
  liftIO $ run 3001 $ simpleCors $ logStdoutDev $ app e

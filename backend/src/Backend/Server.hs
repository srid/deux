{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Backend.Server (runServer) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader

import Data.Text.Lazy as T

import Servant
import Servant.Server (hoistServer)

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Common

import Backend

type AppM = ReaderT Env Handler

server :: ServerT DemoAPI AppM
server = getDemo
  where getDemo :: AppM (Either Text Demo)
        getDemo = do
          _e <- ask
          Right <$> parseDemo -- TODO: wrap in Either, handle exception. how in readerT?
          -- r :: Either SomeException Demo <- try parseDemo
          -- return $ first (T.pack . show) r

nt :: Env -> AppM a -> Handler a
nt s x = runReaderT x s

app :: Env -> Application
app s = serve demoAPI $ hoistServer demoAPI (nt s) server

demoAPI :: Proxy DemoAPI
demoAPI = Proxy

runServer
  :: (Functor m, MonadReader Env m, MonadIO m)
  => m ()
runServer = do
  e <- ask
  liftIO $ putStrLn "Running server at http://localhost:3001/"
  liftIO $ run 3001 $ simpleCors $ logStdoutDev $ app e

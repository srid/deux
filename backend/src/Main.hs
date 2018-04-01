{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Exception (try, SomeException)

import Common
import Backend
import qualified Backend.Server as Server

main :: IO ()
main = do
  _ :: Either SomeException Demo <- try parseDemo
  Server.runServer

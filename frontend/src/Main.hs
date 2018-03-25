{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T

import Diagrams.Backend.Reflex
import Diagrams.Prelude hiding (connect, el, text)

import Servant.API
import Servant.Reflex

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (mainWidget)
import Reflex.Dom.Main (mainWidget)

import Common

main :: IO ()
main = do
  putStrLn "Running app at http://localhost:3000/"
  JW.run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = do
  divClass "" $ do
    el "tt" $ text $ "todo"
  return ()

someWidget :: forall t m. MonadWidget t m => m ()
someWidget = do
  let (demo :<|> _) = client (Proxy :: Proxy DemoAPI)
            (Proxy :: Proxy m)
            (Proxy :: Proxy ())
            (constDyn (BasePath "/demo"))
  demo never
  return ()

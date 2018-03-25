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
import qualified Data.Text as T

import Servant.Reflex

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (mainWidget)
import Reflex.Dom.Main (mainWidget)

import Common

main :: IO ()
main = do
  JW.run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = do
  someWidget
  return ()

serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

someWidget :: forall t m. MonadWidget t m => m ()
someWidget = do
  let (demo) = client (Proxy :: Proxy DemoAPI)
            (Proxy :: Proxy m)
            (Proxy :: Proxy ())
            (constDyn serverUrl)
  pb <- getPostBuild
  result <- demo $ () <$ pb
  let ys = fmapMaybe reqSuccess result
      errs = fmapMaybe reqFailure result

  el "tt" $ do
    dynText =<< holdDyn "" (T.pack . show <$> ys)
  return ()

  elAttr "p" ("style" =: "color:red") $
    dynText =<< holdDyn "" (leftmost [errs, const "" <$> ys])

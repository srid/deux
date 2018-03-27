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

import Control.Monad (void, forM_)
import Data.Proxy (Proxy (..))
import qualified Data.Text
import qualified Data.Text.Lazy as T

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

  tasks <- holdDyn [] ys
  el "tt" $ do
    taskList tasks
  return ()

  elAttr "p" ("style" =: "color:red") $
    dynText =<< holdDyn "" (leftmost [errs, const "" <$> ys])

taskList :: MonadWidget t m => Dynamic t [Task] -> m ()
taskList tasks =
  void $ dyn $ ffor tasks $ \tasks' -> do
    forM_ tasks' task

task :: MonadWidget t m => Task -> m ()
task (Task s done_ desc_) = do
  el "h3" $ text $ T.toStrict s
  el "code" $ text $ tshow desc_

tshow :: Show a => a -> Data.Text.Text
tshow = Data.Text.pack . show

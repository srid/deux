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

import Control.Monad (forM_, void)
import Data.Proxy (Proxy (..))
import qualified Data.Text
import qualified Data.Text.Lazy as T

import Servant.Reflex

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (mainWidgetWithCss)

import Reflex.Dom.SemanticUI

import Common

main :: IO ()
main = do
  JW.run 3000 $ mainWidgetWithCss cssInline app
  where
    cssInline = "@import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.css);";

app :: MonadWidget t m => m ()
app = container def $  do
  someWidget
  return ()

serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

someWidget :: forall t m. MonadWidget t m => m ()
someWidget = do
  let demo = client (Proxy :: Proxy DemoAPI)
            (Proxy :: Proxy m)
            (Proxy :: Proxy ())
            (constDyn serverUrl)
  pb <- getPostBuild
  result <- demo $ () <$ pb
  let ys = fmapMaybe reqSuccess result
      errs = fmapMaybe reqFailure result

  elAttr "p" ("style" =: "color:red") $
    dynText =<< holdDyn "" (leftmost [errs, const "" <$> ys])

  demoData <- holdDyn (Demo [] []) ys

  segment def $ do
    header (def & headerSize |?~ H2) $ text "Pieces"
    void $ dyn $ ffor (_demoPieces <$> demoData) $ \pieces -> forM_ pieces $ \piece -> do
      header (def & headerSize |?~ H3) $ do
        text $ T.toStrict $ _pieceTitle piece
      el "tt" $ text $ T.toStrict $ _pieceBody piece

  segment (def
            & segmentInverted |~ True
            & segmentSize |?~ Small) $ do
    header (def & headerSize |?~ H2) $ text "Tasks"
    el "tt" $ do
      taskList $ _demoTasks <$> demoData
    return ()

taskList :: MonadWidget t m => Dynamic t [Task] -> m ()
taskList tasks =
  void $ dyn $ ffor tasks $ \tasks' -> do
    forM_ tasks' task

task :: MonadWidget t m => Task -> m ()
task (Task s _done ctx desc_) = do
  header (def & headerSize |?~ H3
            & headerInverted |~ True
            & headerColor |?~ Purple) $ do
    text $ T.toStrict s
    text $ T.toStrict $ T.pack $ show ctx
  el "tt" $ text $ T.toStrict desc_


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
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM_, join, (<=<))
import Data.Bool (bool)
import Data.Proxy (Proxy (..))
import Data.Set (fromList, toList)
import qualified Data.Text.Lazy as T

import Servant.Reflex

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (button, mainWidgetWithCss, _dropdown_value)

import Reflex.Dom.SemanticUI

import Common

main :: IO ()
main = do
  -- TODO: Multiplex when doing ghcjs builds
  JW.run 3000 $ mainWidgetWithCss cssInline app
  where
    cssInline =
      "@import url(https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.css);\
      \.asis { font-family: monospace; white-space: pre-wrap; }";

app :: MonadWidget t m => m ()
app = container def $  do
  someWidget
  return ()

serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

serverEvt :: forall t m. MonadWidget t m => m (Event t (ReqResult () Demo))
serverEvt = do
  pb <- getPostBuild
  demo $ () <$ pb
  where
    demo = client
      (Proxy :: Proxy DemoAPI)
      (Proxy :: Proxy m)
      (Proxy :: Proxy ())
      (constDyn serverUrl)

someWidget :: forall t m. MonadWidget t m => m ()
someWidget = do
  result <- serverEvt
  let ys = fmapMaybe reqSuccess result
      errs = fmapMaybe reqFailure result

  elAttr "p" ("style" =: "color:red") $
    dynText =<< holdDyn "" (leftmost [errs, const "" <$> ys])

  widgetHold_ blank $ taskList <$> _demoTasks <$> ys
  widgetHold_ blank $ pieceList <$> _demoPieces <$> ys

taskList :: MonadWidget t m => [Task] -> m ()
taskList tasks = segment def $ do
  header (def & headerSize |?~ H2) $ text "Current tasks"
  ctxQuery <- taskFilter $ allContexts tasks
  dyn_ $ ffor ctxQuery $ \q ->
    segment (def & segmentRaised |~ True) $ do
      forM_ tasks $ \t -> do
        if matchCtx q t
          then task t
          else blank

matchCtx :: T.Text -> Task -> Bool
matchCtx c t = case c of
  "" -> True
  _ -> any (T.isInfixOf c) $ _taskContext t

allContexts :: [Task] -> [T.Text]
allContexts = toList . fromList . join . fmap _taskContext

taskFilter :: MonadWidget t m => [T.Text] -> m (Dynamic t T.Text)
taskFilter ctxs = do
  d <- _dropdown_value <<$>> searchDropdown def "" $ TaggedStatic $ T.toStrict <$> ctxs
  return $ T.fromStrict <$> d

task :: MonadWidget t m => Task -> m ()
task (Task s _done ctx desc) = do
  label (def & labelLink .~ True & labelColor |?~ Teal
              & labelRibbon |?~ LeftRibbon) $ text $ T.toStrict $ T.pack $ show ctx

  rec let conf = def
            & buttonFloated |?~ RightFloated
            & buttonEmphasis .~ Dyn (bool Nothing (Just Primary) <$> viewNote)
      viewNote <- if desc /= ""
                     then toggle False <=< button conf $ text "Toggle Notes"
                     else return $ constDyn False

  text $ T.toStrict s
  dyn_ $ ffor viewNote $ \case
    True -> note desc
    False -> blank
  divider def

pieceList :: MonadWidget t m => [Piece] -> m ()
pieceList pieces = segment def $ do
  header (def & headerSize |?~ H2) $ text "Pieces"
  forM_ pieces $ \piece -> do
    header (def & headerSize |?~ H3) $ do
      text $ T.toStrict $ _pieceTitle piece
    note $ _pieceBody piece

note :: UI t m => T.Text -> m ()
note = segment def . divClass "asis" . text . T.toStrict

(<<$>>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<$) :: (Functor f2, Functor f1) => a -> f1 (f2 b) -> f1 (f2 a)
v <<$ f = fmap (v <$) f

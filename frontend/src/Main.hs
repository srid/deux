{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import qualified Data.Text.Lazy as TL

import Servant.Reflex

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (button, mainWidgetWithCss, _dropdown_value)
import Reflex.Dom.SemanticUI

import Common
import Frontend.Common (tabs_)
import qualified Frontend.Piece as Piece
import qualified Frontend.Task as Task

-- TODO: Start using ReaderT
serverUrl :: BaseUrl
serverUrl = BaseFullUrl Http "localhost" 3001 "/"

data Tab
  = Tab_Tasks
  | Tab_Pieces
  deriving (Eq, Show, Ord)

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
  result <- getPostBuild >>= donnesClient . fmap (const ())
  let ys = fmapMaybe reqSuccess result
      errs = fmapMaybe reqFailure result

  elAttr "p" ("style" =: "color:red") $
    dynText =<< holdDyn "" (leftmost [errs, const "" <$> ys])

  widgetHold_ (text "Loading...") $ ffor ys $ \case
    Left e -> do
      label def $ do
      divClass "asis" $ text $ "oops:\n " <> TL.toStrict e
    Right d -> do
      tabs_ Tab_Tasks
        [ (Tab_Tasks, text "Tasks")
        , (Tab_Pieces, text "Pieces")
        ] $ \case
        Tab_Tasks -> workflowView $ Task.taskList $ _donnesTasks d
        Tab_Pieces -> workflowView $ Piece.pieceList $ _donnesPieces d

donnesClient
  :: forall t m. MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () (Either TL.Text Donnees)))
donnesClient = client
  (Proxy :: Proxy DonneesAPI)
  (Proxy :: Proxy m)
  (Proxy :: Proxy ())
  (constDyn serverUrl)

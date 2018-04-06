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

import Control.Monad (forM_, join, (<=<))
import Data.Bool (bool)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Set (fromList, toList)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import GHCJS.DOM.Types (MonadJSM)
import Servant.Reflex

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (button, mainWidgetWithCss, _dropdown_value)
import Reflex.Dom.SemanticUI

import Common
import Frontend.Common (withWorkflow, tabs_)

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
        Tab_Tasks -> workflowView $ taskList $ _donnesTasks d
        Tab_Pieces -> workflowView $ pieceList $ _donnesPieces d

donnesClient
  :: forall t m. MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () (Either TL.Text Donnees)))
donnesClient = client
  (Proxy :: Proxy DonneesAPI)
  (Proxy :: Proxy m)
  (Proxy :: Proxy ())
  (constDyn serverUrl)

pieceList :: UI t m => [Piece] -> Workflow t m ()
pieceList pieces = withWorkflow $ segment def $ do
  header (def & headerConfig_size |?~ H2) $ text "Pieces"
  forM_ pieces $ \piece -> do
    header (def & headerConfig_size |?~ H3) $ do
      text $ TL.toStrict $ _pieceTitle piece
    note $ _pieceBody piece
  return never

taskList
  :: ( UI t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadJSM (Performable m)
     , MonadJSM m)
  => [Task]
  -> Workflow t m ()
taskList tasks = withWorkflow $ segment def $ do
  header (def & headerConfig_size |?~ H2) $ text "Current tasks"
  ctxQuery <- taskFilter $ allContexts tasks
  dyn_ $ ffor ctxQuery $ \q ->
    segment (def & segmentConfig_raised |~ True) $ do
      forM_ tasks $ \t -> do
        if matchCtx q t
          then task t
          else blank
  return never

matchCtx :: TL.Text -> Task -> Bool
matchCtx c t = case c of
  "" -> True
  _ -> any (TL.isInfixOf c) $ _taskContext t

allContexts :: [Task] -> [TL.Text]
allContexts = toList . fromList . join . fmap _taskContext

taskFilter
  :: ( UI t m
     , DomBuilderSpace m ~ GhcjsDomSpace
     , MonadJSM (Performable m)
     , MonadJSM m)
  => [TL.Text] -> m (Dynamic t TL.Text)
taskFilter = toLazy . f . toStrict
  where
    f = fmap _dropdown_value . searchDropdown def "" . TaggedStatic
    toStrict = fmap TL.toStrict
    toLazy = fmap . fmap $ TL.fromStrict

task :: UI t m => Task -> m ()
task (Task s _done ctx desc) = do
  label (def & labelConfig_link .~ True
             & labelConfig_color |?~ Teal
             & labelConfig_ribbon |?~ LeftRibbon) $
    text $ TL.toStrict $ TL.pack $ show ctx

  text $ TL.toStrict s

  viewNote <- if desc /= ""
    then toggleButton "Toggle Notes"
    else return $ pure False

  dyn_ $ ffor viewNote $ \case
    True -> note desc
    False -> blank
  divider def

toggleButton :: UI t m => T.Text -> m (Dynamic t Bool)
toggleButton s = do
  rec let conf = def
            & buttonConfig_floated |?~ RightFloated
            & buttonConfig_emphasis .~ Dyn (bool Nothing (Just Primary) <$> viewNote)
      viewNote <- toggle False <=< button conf $ text s
  return viewNote


note :: UI t m => TL.Text -> m ()
note = segment def . divClass "asis" . text . TL.toStrict


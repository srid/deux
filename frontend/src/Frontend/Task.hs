{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Task where

import Control.Monad (forM_, join)
import Data.Set (fromList, toList)
import qualified Data.Text.Lazy as TL

import GHCJS.DOM.Types (MonadJSM)
import Reflex.Dom hiding (button, mainWidgetWithCss, _dropdown_value)
import Reflex.Dom.SemanticUI

import Common.Task
import Frontend.Common (toggleButton, withWorkflow)
import Frontend.Markdown (markdown)

taskList :: MonadWidget t m  => [Task] -> Workflow t m ()
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

task :: MonadWidget t m => Task -> m ()
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
    True -> markdown $ constDyn $ TL.toStrict desc
    False -> blank
  divider def


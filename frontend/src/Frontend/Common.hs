{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Common where

import Data.Bool (bool)
import Data.Semigroup ((<>))
import Control.Monad (forM, (<=<))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Reflex.Dom hiding (button, mainWidgetWithCss, _dropdown_value)
import Reflex.Dom.SemanticUI

-- | Wrap a workflow creating a widget for use other workflows
withWorkflow :: (Monoid a, UI t m) => m (Event t (Workflow t m a)) -> Workflow t m a
withWorkflow = Workflow . fmap (mempty,)

-- | End the workflow with the given value (no more further events)
endWorkflow :: UI t m => a -> Workflow t m a
endWorkflow x = Workflow $ return (x, never)

(<<$>>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<$) :: (Functor f2, Functor f1) => a -> f1 (f2 b) -> f1 (f2 a)
v <<$ f = fmap (v <$) f

toggleButton :: UI t m => T.Text -> m (Dynamic t Bool)
toggleButton s = do
  rec let conf = def
            & buttonConfig_floated |?~ RightFloated
            & buttonConfig_emphasis .~ Dyn (bool Nothing (Just Primary) <$> viewNote)
      viewNote <- toggle False <=< button conf $ text s
  return viewNote

note :: UI t m => TL.Text -> m ()
note = segment def . divClass "asis" . text . TL.toStrict

tabs_
  :: (UI t m, Eq tab)
  => tab -> [(tab, m ())] -> (tab -> m a) -> m ()
tabs_ tab0 tabHeaders renderTab = do
  rec
    currentTab <- holdDyn tab0 openTab
    openTab <- tabbedMenu $
      fmap leftmost $ forM tabHeaders $ \(tab, tabHeader) -> do
        let active = ffor currentTab (== tab)
        tab <<$ tabItem active tabHeader
  segment (def & segmentConfig_attached |?~ BottomAttached) $ do
    widgetHold_ (renderTab tab0) $ ffor openTab renderTab
  where
    tabItem active w
      = (domEvent Click . fst) <<$>> elDynClass' "a" classes_ $ w
      where
        classes_ = ("ui item" <>) <$> (bool "" " active" <$> active)
    tabbedMenu
      = menu $ def
          & menuConfig_elConfig .~ (def & classes |~ "top attached stackable tabular")

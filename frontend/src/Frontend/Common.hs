{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Frontend.Common where

import Reflex.Dom hiding (button, mainWidgetWithCss, _dropdown_value)

-- | Wrap a workflow creating a widget for use other workflows
withWorkflow :: (Monoid a, MonadWidget t m) => m (Event t (Workflow t m a)) -> Workflow t m a
withWorkflow = Workflow . fmap (mempty,)

-- | End the workflow with the given value (no more further events)
endWorkflow :: MonadWidget t m => a -> Workflow t m a
endWorkflow x = Workflow $ return (x, never)

(<<$>>) :: (Functor f2, Functor f1) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<$) :: (Functor f2, Functor f1) => a -> f1 (f2 b) -> f1 (f2 a)
v <<$ f = fmap (v <$) f


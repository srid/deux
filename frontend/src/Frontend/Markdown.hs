{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Markdown (markdown) where

import Control.Monad (void)
import Data.Monoid ((<>))
import Data.Profunctor (dimap)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Pandoc

import Reflex.Dom.SemanticUI

markdown :: MonadWidget t m => Dynamic t Text -> m ()
markdown = segment def . void . elDynHtml' "div" . fmap markdownToHtml

markdownToHtml :: Text -> Text
markdownToHtml = dimap inp out markdownToHtml'
  where
    inp = T.unpack
    out = either (("Error: " <>) . tshow) T.pack

markdownToHtml' :: String -> Either PandocError String
markdownToHtml' s = writeHtmlString opts <$> readMarkdown def s
  where
    opts = def { writerHtml5 = True }

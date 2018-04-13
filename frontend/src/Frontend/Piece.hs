{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Piece where

import Control.Monad (forM_)
import qualified Data.Text.Lazy as TL

import Text.Pandoc

import Reflex.Dom.SemanticUI

import Common.Piece
import Frontend.Common (withWorkflow)
import Frontend.Markdown (markdown)

pieceList
  :: MonadWidget t m
  => [Piece] -> Workflow t m ()
pieceList pieces = withWorkflow $ segment def $ do
  header (def & headerConfig_size |?~ H2) $ text "Pieces"
  forM_ pieces $ \piece -> do
    header (def & headerConfig_size |?~ H3) $ 
      text $ TL.toStrict $ _pieceTitle piece
    markdown $ constDyn $ TL.toStrict $ _pieceBody piece
  return never

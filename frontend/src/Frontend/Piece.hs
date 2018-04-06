{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Frontend.Piece where

import Control.Monad (forM_)
import qualified Data.Text.Lazy as TL

import Reflex.Dom hiding (button, mainWidgetWithCss, _dropdown_value)
import Reflex.Dom.SemanticUI

import Common.Piece
import Frontend.Common (withWorkflow, note)

pieceList :: UI t m => [Piece] -> Workflow t m ()
pieceList pieces = withWorkflow $ segment def $ do
  header (def & headerConfig_size |?~ H2) $ text "Pieces"
  forM_ pieces $ \piece -> do
    header (def & headerConfig_size |?~ H3) $ do
      text $ TL.toStrict $ _pieceTitle piece
    note $ _pieceBody piece
  return never

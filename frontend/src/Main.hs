{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

import Diagrams.Backend.Reflex
import Diagrams.Prelude hiding (el, text, connect)

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (mainWidget)
import Reflex.Dom.Main (mainWidget)

import Algebra.Graph.AdjacencyMap

type MindMap = AdjacencyMap Text

data Category
  = ToDo
  | Done
  | Project
  deriving (Eq, Show, Ord)

data Node cat where
  Node_Category :: Category -> Node Category
  Node_Text :: Text -> Node Text

mindMap :: AdjacencyMap (Node a)
mindMap = connect g1 g2
  where g1 = edge (Node_Category ToDo) (Node_Text "plan out MVP")
        g2 = edge (Node_Category Project) (Node_Text "Project: Deux")


mindMap' = connect g1 g2
  where g1 = edge "todo" "brush teeth"
        g2 = edge "todo" "plan out MVP"
        g3 = edge "todo" "Project: Deux"

-- The idea is to build a gigantic graph of all stuff; store it in a simple
-- file; have the backend serve it; for Reflex to render it.

-- Rendering is a challenge of its own.
--  > Render a sub graph of path 6 from a given a node
--  > User clicking a node makes that the starting node in UI
-- How do we render this? One way is to have the user explicitly specify the 'direction'.
-- Or, a graph pattern. Ex: a list pattern that renders vertically (normal direction) or
-- horizontally.

-- Think about a way to draw the graph in 2D.
drawGraph :: Show a => AdjacencyMap a -> m ()
drawGraph = undefined

main :: IO ()
main = do
  putStrLn "Running app at http://localhost:3000/"
  JW.run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = do
  let sz = dims2D 100.0 100.0
      opts = ReflexOptions sz mempty
  ev <- reflexDia opts diagram
  divClass "" $ do
    el "tt" $ text $ T.pack $ show mindMap
  return ()

hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)

diagram :: Diagram B
diagram = strokeT (hilbert 6) # lc green
                              # opacity 0.3

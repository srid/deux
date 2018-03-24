{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

import Diagrams.Backend.Reflex
import Diagrams.Prelude

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (mainWidget)
import Reflex.Dom.Main (mainWidget)

-- Create a runner that watches for ghcid success, and reloads the app!
main :: IO ()
main = do
  putStrLn "Running app at http://localhost:3000/"
  JW.run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = do
  let sz = dims2D 500.0 500.0
      opts = ReflexOptions sz mempty
  ev <- reflexDia opts diagram
  return ()

hilbert 0 = mempty
hilbert n = hilbert' (n-1) # reflectY <> vrule 1
         <> hilbert  (n-1) <> hrule 1
         <> hilbert  (n-1) <> vrule (-1)
         <> hilbert' (n-1) # reflectX
  where
    hilbert' m = hilbert m # rotateBy (1/4)

diagram :: Diagram B
diagram = strokeT (hilbert 6) # lc blue
                              # opacity 0.3

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

main :: IO ()
main = do
  putStrLn "Running app at http://localhost:3000/"
  JW.run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = do
  divClass "" $ do
    el "tt" $ text $ "todo"
  return ()

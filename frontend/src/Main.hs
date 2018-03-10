{-# LANGUAGE OverloadedStrings #-} module Main where

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom.Main (mainWidget)
import Reflex.Dom hiding (mainWidget)

main :: IO ()
main = JW.run 3000 $ mainWidget $ text "hi"

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T

import Diagrams.Backend.Reflex
import Diagrams.Prelude

import qualified Language.Javascript.JSaddle.Warp as JW
import Reflex.Dom hiding (mainWidget)
import Reflex.Dom.Main (mainWidget)

main :: IO ()
main = JW.run 3000 $ mainWidget app

app :: MonadWidget t m => m ()
app = do
  ev <- reflexDia def (circle 100 # lc blue # lwL 2)
  ct <- count . ffilter getAny $ diaMousedownEv ev
  dynText $ fmap counter ct
  return ()

counter :: Int -> Text
counter i = T.concat ["The circle has been clicked ", T.pack (show i), " times" ]


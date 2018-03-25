{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Dhall

import Common

main :: IO ()
main = do
  x <- input auto "../demo.dhall"
  print (x :: Task)

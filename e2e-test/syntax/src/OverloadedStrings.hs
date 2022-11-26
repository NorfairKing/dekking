{-# LANGUAGE OverloadedStrings #-}

module OverloadedStrings where

import Data.Text (Text)

main :: IO ()
main = do
  let printString :: String -> IO ()
      printString = print
  let printText :: Text -> IO ()
      printText = print
  printString "Hi"
  printText "Hi"

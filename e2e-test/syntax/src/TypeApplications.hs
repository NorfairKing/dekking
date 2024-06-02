{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module TypeApplications where

import Data.String

main :: IO ()
main = do
  print (noArguments @String)
  print (oneArgument @String "hi")
  print (twoArguments @String "hi" "ho")

noArguments :: (IsString a) => a
noArguments = "no arguments"

oneArgument :: (IsString a, Semigroup a) => a -> a
oneArgument s = "one arguments" <> s

twoArguments :: (IsString a, Semigroup a) => a -> a -> a
twoArguments s1 s2 = "one arguments" <> s1 <> s2

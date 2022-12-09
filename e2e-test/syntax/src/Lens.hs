{-# HLINT ignore #-}
module Lens where

import Lens.Micro

main :: IO ()
main = do
  let example = Example {exampleString = "hi"}
  print $ example & exampleStringL .~ "ho"
  print $ example ^. exampleStringL
  print $ [example] ^? ix 2

data Example = Example {exampleString :: String}
  deriving (Show)

-- Making sure that existential types still type-check after the source-transformation.
exampleStringL :: Lens' Example String
exampleStringL = lens exampleString (\e s -> e {exampleString = id s})

{-# HLINT ignore #-}
-- This would probably fix it:
-- {-# LANGUAGE DeepSubsumption #-}
module Lens where

import Lens.Micro

main :: IO ()
main = do
  print $ Example {exampleString = "hi"} & exampleStringL .~ "ho"

data Example = Example {exampleString :: String}
  deriving (Show)

-- Making sure that existential types still type-check after the source-transformation.
exampleStringL :: Lens' Example String
exampleStringL = lens exampleString (\e s -> e {exampleString = id s})

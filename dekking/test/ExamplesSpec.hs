module ExamplesSpec (spec) where

import Dekking.Coverable
import Test.Syd

spec :: Spec
spec = do
  it "Makes the same coverables for TopLevel.hs" $ do
    goldenStringFile
      "test_resources/Examples/TopLevel.hs.coverable"
      (readFile "test/Examples/TopLevel.hs.coverable")

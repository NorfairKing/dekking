{-# LANGUAGE TypeApplications #-}

import Foobar
import Foobar.Gen ()
import Test.Syd
import Test.Syd.Validity

main :: IO ()
main = sydTest $ do
  it "can print this example" $
    printExample
      Example
        { exampleString = "hi",
          exampleInt = 42
        }
  genValidSpec @Example

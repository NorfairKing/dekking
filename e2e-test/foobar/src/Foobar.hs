{-# LANGUAGE DeriveGeneric #-}

module Foobar where

import Data.Validity
import GHC.Generics (Generic)

data Example = Example
  { exampleString :: String,
    exampleInt :: Int
  }
  deriving (Show, Eq, Generic)

instance Validity Example

printExample :: Example -> IO ()
printExample = print

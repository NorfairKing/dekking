{-# LANGUAGE DefaultSignatures #-}

module Typeclass where

class ExampleClass a where
  exampleString :: a -> String
  default exampleString :: Show a => a -> String
  exampleString = show

instance ExampleClass Int where
  exampleString = show

instance ExampleClass Double

main :: IO ()
main = do
  putStrLn $ exampleString (5 :: Int)
  putStrLn $ exampleString (5 :: Double)

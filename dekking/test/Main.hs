{-# OPTIONS_GHC -fplugin=Dekking -ddump-parsed -ddump-rn #-}

module Main (main) where

main :: IO ()
main = do
  putStrLn "Hi"
  print test

test :: Int
test = 5

uncoveredFunction :: Char
uncoveredFunction = 'a'

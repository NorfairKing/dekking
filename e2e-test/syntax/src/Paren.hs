{-# HLINT ignore #-}
module Paren where

main :: IO ()
main = do
  -- Parentheses are respected
  putStrLn ("Hello" <> "world")

  -- Currying works correctly
  print ((+) ((+) 5 6) 7)

  -- Operators are respected
  -- See [ref:NoUniplate]
  print (7 + 8 + 9)

  -- Custom operators are respected
  print (3 +++ 4 +++ 5)

  -- Infix operators work fine
  let f = (+)
  print (5 `f` 2)

  -- Multiple dollars work just fine
  print $ succ $ 5

(+++) = (+)

module Examples.Paren where

main :: IO ()
main =
  -- -- Parentheses are respected
  -- putStrLn ("Hello" <> "world")

  -- -- Currying works correctly
  -- print ((+) ((+) 5 6) 7)

  -- -- Operators are respected
  -- print (7 + 8 + 9)

  -- -- Custom operators are respected
  -- print (3 +++ 4 +++ 5)

  -- -- Infix operators work fine
  -- print (5 `exp` 2)

  -- Dollars work just fine
  print $ succ $ 5

-- (+++) = (+)

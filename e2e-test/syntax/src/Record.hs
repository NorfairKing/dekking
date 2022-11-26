module Record where

main :: IO ()
main = do
  print $ addOne $ Example 1

newtype Example = Example {unExample :: Int}
  deriving (Show)

addOne :: Example -> Example
addOne e = e {unExample = unExample e + 1}

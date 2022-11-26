import qualified OverloadedStrings
import qualified Paren
import qualified Record
import qualified TopLevel

main :: IO ()
main = do
  TopLevel.main
  Paren.main
  OverloadedStrings.main
  Record.main

import qualified OverloadedStrings
import qualified Paren
import qualified Record
import qualified TopLevel
import qualified TypeApplications

main :: IO ()
main = do
  OverloadedStrings.main
  Paren.main
  Record.main
  TopLevel.main
  TypeApplications.main

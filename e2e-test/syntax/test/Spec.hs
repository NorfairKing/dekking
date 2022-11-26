import qualified OverloadedStrings
import qualified Paren
import qualified Record
import qualified TopLevel

main :: IO ()
main = do
  let don't _ = pure ()

  -- TopLevel
  TopLevel.covered
  TopLevel.coveredWithArg 5
  don't TopLevel.uncovered
  don't $ TopLevel.uncoveredWithArg 5

  Paren.main

  OverloadedStrings.main

  Record.main

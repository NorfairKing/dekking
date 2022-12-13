import qualified Annotations
import qualified Lens
import qualified OverloadedStrings
import qualified Paren
import qualified Record
import qualified ServantExample
import qualified TopLevel
import qualified TypeApplications
import qualified Typeclass

main :: IO ()
main = do
  Annotations.main
  Lens.main
  OverloadedStrings.main
  Paren.main
  Record.main
  ServantExample.main
  TopLevel.main
  TypeApplications.main
  Typeclass.main

module Dekking.Coverage where

import Data.Set (Set)
import qualified Data.Set as S
import Path

type TopLevelBinding = String

readCoverageFile :: Path Abs File -> IO (Set TopLevelBinding)
readCoverageFile p = S.fromList . lines <$> readFile (fromAbsFile p)

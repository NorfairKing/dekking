module Dekking.Coverage where

import Path

type TopLevelBinding = String

readCoverageFile :: Path Abs File -> IO [TopLevelBinding]
readCoverageFile p = lines <$> readFile (fromAbsFile p)

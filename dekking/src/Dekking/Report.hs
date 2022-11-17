{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain) where

import Data.List
import Dekking.Coverable
import Dekking.Coverage
import Path
import Path.IO
import Text.Show.Pretty

reportMain :: IO ()
reportMain = do
  here <- getCurrentDir
  fs <- snd <$> listDirRecur here
  coverage <-
    foldMap readCoverageFile $
      filter ((== [relfile|coverage.dat|]) . filename) fs
  coverables <-
    foldMap readCoverableFile $
      filter (maybe False (isSuffixOf "coverable") . fileExtension) fs

  pPrint coverables
  pPrint coverage
  pPrint (computeCoverageReport coverables coverage)

computeCoverageReport :: Coverables -> [TopLevelBinding] -> CoverageReport
computeCoverageReport Coverables {..} topLevelCoverage =
  CoverageReport
    { coverageReportTopLevelBindings = computeCoverage coverableTopLevelBindings topLevelCoverage
    }

data CoverageReport = CoverageReport {coverageReportTopLevelBindings :: Coverage String}
  deriving (Show, Eq)

data Coverage a = Coverage
  { coverageCovered :: [a],
    coverageUncovered :: [a]
  }
  deriving (Show, Eq)

computeCoverage :: Eq a => [a] -> [a] -> Coverage a
computeCoverage coverables covereds =
  Coverage
    { coverageCovered = coverables `intersect` covereds,
      coverageUncovered = coverables \\ covereds
    }

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain) where

import Data.List
import Dekking.Coverable
import Dekking.Coverage
import Dekking.OptParse
import Path
import Path.IO
import System.Environment (getArgs)
import Text.Show.Pretty

reportMain :: IO ()
reportMain = do
  Settings {..} <- getSettings

  coverablesFiles <- concat <$> mapM (fmap snd . listDirRecur) settingCoverablesDirs
  coverables <-
    foldMap readCoverableFile $
      filter
        (maybe False (isSuffixOf "coverable") . fileExtension)
        coverablesFiles

  coverage <- foldMap readCoverageFile settingCoverageFiles

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

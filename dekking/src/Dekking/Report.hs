{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain) where

import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Dekking.Coverable
import Dekking.Coverage
import Dekking.OptParse
import Path
import Path.IO
import Text.Show.Pretty

reportMain :: IO ()
reportMain = do
  Settings {..} <- getSettings

  coverablesFiles <-
    filter
      (maybe False (isSuffixOf "coverable") . fileExtension)
      . concat
      <$> mapM (fmap snd . listDirRecur) (S.toList settingCoverablesDirs)

  let coverageFiles = settingCoverageFiles

  coverables <-
    flip foldMap coverablesFiles $ \coverablesFile -> do
      print coverablesFile
      coverables <- readCoverableFile coverablesFile
      pPrint coverables
      pure coverables

  coverage <-
    flip foldMap coverageFiles $ \coverageFile -> do
      print coverageFile
      coverage <- readCoverageFile coverageFile
      pPrint coverage
      pure coverage

  pPrint coverables
  pPrint coverage
  pPrint (computeCoverageReport coverables coverage)

computeCoverageReport :: Coverables -> Set TopLevelBinding -> CoverageReport
computeCoverageReport Coverables {..} topLevelCoverage =
  CoverageReport
    { coverageReportTopLevelBindings =
        computeCoverage coverableTopLevelBindings topLevelCoverage
    }

data CoverageReport = CoverageReport
  { coverageReportTopLevelBindings :: Coverage TopLevelBinding
  }
  deriving (Show, Eq)

data Coverage a = Coverage
  { coverageCovered :: Set (Coverable a),
    coverageUncovered :: Set (Coverable a)
  }
  deriving (Show, Eq)

computeCoverage :: Ord a => Set (Coverable a) -> Set a -> Coverage a
computeCoverage coverables covereds =
  Coverage
    { coverageCovered = S.filter ((`S.member` covereds) . coverableValue) coverables,
      coverageUncovered = S.filter (not . (`S.member` covereds) . coverableValue) coverables
    }

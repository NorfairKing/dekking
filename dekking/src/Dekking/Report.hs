{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain, computeModuleCoverageReport) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List
import Data.Map (Map)
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
      (maybe False (isSuffixOf "coverables") . fileExtension)
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
  pPrint (computeModuleCoverageReport coverables coverage)

computeModuleCoverageReport :: Coverables -> Set TopLevelBinding -> ModuleCoverageReport
computeModuleCoverageReport Coverables {..} topLevelCoverage =
  ModuleCoverageReport
    { moduleCoverageReportTopLevelBindings =
        computeCoverage coverableTopLevelBindings topLevelCoverage
    }

newtype CoverageReport = CoverageReport {coverageReportModules :: Map String ModuleCoverageReport}
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec CoverageReport)

instance HasCodec CoverageReport where
  codec = dimapCodec CoverageReport coverageReportModules codec

data ModuleCoverageReport = ModuleCoverageReport
  { moduleCoverageReportTopLevelBindings :: Coverage TopLevelBinding
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ModuleCoverageReport)

instance HasCodec ModuleCoverageReport where
  codec =
    object "ModuleCoverageReport" $
      ModuleCoverageReport
        <$> requiredField "top-level-bindings" "top level bindings" .= moduleCoverageReportTopLevelBindings

data Coverage a = Coverage
  { coverageCovered :: Set (Coverable a),
    coverageUncovered :: Set (Coverable a)
  }
  deriving (Show, Eq, Ord)

instance (Ord a, HasCodec a) => HasCodec (Coverage a) where
  codec =
    object "Coverage" $
      Coverage
        <$> requiredField "covered" "covered values" .= coverageCovered
        <*> requiredField "uncovered" "uncovered values" .= coverageUncovered

computeCoverage :: Ord a => Set (Coverable a) -> Set a -> Coverage a
computeCoverage coverables covereds =
  Coverage
    { coverageCovered = S.filter ((`S.member` covereds) . coverableValue) coverables,
      coverageUncovered = S.filter (not . (`S.member` covereds) . coverableValue) coverables
    }

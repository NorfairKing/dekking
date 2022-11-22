{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain, computeCoverageReport, computeModuleCoverageReport) where

import Autodocodec
import Control.Arrow (left)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Dekking.Coverable
import Dekking.Coverage
import Dekking.OptParse
import Path
import Text.Show.Pretty

reportMain :: IO ()
reportMain = do
  Settings {..} <- getSettings

  coverables <- readCoverablesFiles settingCoverablesDirs
  coverage <- readCoverageFiles settingCoverageFiles

  pPrint coverables
  pPrint coverage
  pPrint (computeCoverageReport coverables coverage)

computeCoverageReport :: Coverables -> Set (Maybe ModuleName, TopLevelBinding) -> CoverageReport
computeCoverageReport Coverables {..} topLevelCoverage =
  CoverageReport $
    M.mapWithKey
      ( \moduleName moduleCoverables ->
          let relevantCoverage =
                S.fromList
                  . mapMaybe
                    ( \(mm, tlb) ->
                        if mm == Just moduleName
                          then Just tlb
                          else Nothing
                    )
                  . S.toList
                  $ topLevelCoverage
           in computeModuleCoverageReport moduleCoverables relevantCoverage
      )
      coverablesModules

newtype CoverageReport = CoverageReport {coverageReportModules :: Map ModuleName ModuleCoverageReport}
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec CoverageReport)

instance HasCodec CoverageReport where
  codec = dimapCodec CoverageReport coverageReportModules codec

computeModuleCoverageReport :: ModuleCoverables -> Set TopLevelBinding -> ModuleCoverageReport
computeModuleCoverageReport ModuleCoverables {..} topLevelCoverage =
  ModuleCoverageReport
    { moduleCoverageReportTopLevelBindings = computeCoverage moduleCoverablesTopLevelBindings topLevelCoverage
    }

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

data Covered = Covered | Uncovered | Uncoverable
  deriving (Show, Eq, Generic)

newtype AnnotatedSource = AnnotatedSource {unAnnotatedSource :: [(String, Covered)]}
  deriving (Show, Eq, Generic)

produceAnnotatedSource :: Coverage TopLevelBinding -> [(String, Covered)]
produceAnnotatedSource = undefined

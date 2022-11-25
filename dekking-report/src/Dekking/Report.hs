{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dekking.Report (reportMain, computeCoverageReport, computeModuleCoverageReport) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Dekking.Coverable
import Dekking.Coverage
import Dekking.OptParse
import Path
import Path.IO
import Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Hamlet
import Text.Lucius
import Text.Printf
import Text.Show.Pretty (pPrint)

reportMain :: IO ()
reportMain = do
  Settings {..} <- getSettings

  coverables <- readCoverablesFiles settingCoverablesDirs
  coverage <- readCoverageFiles settingCoverageFiles

  pPrint coverables
  pPrint coverage
  let coverageReport = computeCoverageReport coverables coverage
  pPrint coverageReport
  ensureDir settingOutputDir
  reportFile <- resolveFile settingOutputDir "report.html"
  styleFile <- resolveFile settingOutputDir "style.css"
  SB.writeFile (fromAbsFile reportFile) $ LB.toStrict $ Blaze.renderHtml $ htmlCoverageReport coverageReport
  SB.writeFile (fromAbsFile styleFile) $ TE.encodeUtf8 coverageReportCss

htmlCoverageReport :: CoverageReport -> Html
htmlCoverageReport CoverageReport {..} = foldMap (uncurry htmlModuleCoverageReport) (M.toList coverageReportModules)

coverageReportCss :: Text
coverageReportCss = LT.toStrict $ renderCss $ $(luciusFile "templates/style.lucius") (error "unused so far")

htmlModuleCoverageReport :: ModuleName -> ModuleCoverageReport -> Html
htmlModuleCoverageReport moduleName ModuleCoverageReport {..} =
  let annotatedLines = zip [(1 :: Word) ..] (unAnnotatedSource moduleCoverageReportAnnotatedSource)
      fmtLineNum :: Word -> String
      fmtLineNum = printf ("%" <> show (floor (logBase 10 (fromIntegral (length annotatedLines) :: Float)) + 1 :: Int) <> "d")
      CoverageSummary {..} = computeCoverageSummary moduleCoverageReportTopLevelBindings
   in $(hamletFile "templates/module.hamlet") (error "unused so far")

coveredColour :: Covered -> Maybe String
coveredColour = \case
  Covered -> Just "#00aa00"
  Uncovered -> Just "yellow"
  Uncoverable -> Nothing

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
  let coverage = computeCoverage moduleCoverablesTopLevelBindings topLevelCoverage
   in ModuleCoverageReport
        { moduleCoverageReportAnnotatedSource = produceAnnotatedSource moduleCoverablesSource coverage,
          moduleCoverageReportTopLevelBindings = coverage
        }

data ModuleCoverageReport = ModuleCoverageReport
  { moduleCoverageReportAnnotatedSource :: AnnotatedSource,
    moduleCoverageReportTopLevelBindings :: Coverage TopLevelBinding
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ModuleCoverageReport)

instance HasCodec ModuleCoverageReport where
  codec =
    object "ModuleCoverageReport" $
      ModuleCoverageReport
        <$> requiredField "annotated-source" "annotated source" .= moduleCoverageReportAnnotatedSource
        <*> requiredField "top-level-bindings" "top level bindings" .= moduleCoverageReportTopLevelBindings

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

data CoverageSummary = CoverageSummary
  { coverageSummaryTotal :: !Word,
    coverageSummaryCovered :: !Word,
    coverageSummaryUncovered :: !Word
  }
  deriving (Show, Eq, Ord)

computeCoverageSummary :: Coverage a -> CoverageSummary
computeCoverageSummary Coverage {..} =
  let coverageSummaryCovered = fromIntegral $ S.size coverageCovered
      coverageSummaryUncovered = fromIntegral $ S.size coverageUncovered
      coverageSummaryTotal = coverageSummaryCovered + coverageSummaryUncovered
   in CoverageSummary {..}

newtype AnnotatedSource = AnnotatedSource {unAnnotatedSource :: [[(String, Covered)]]}
  deriving (Show, Eq)

instance HasCodec AnnotatedSource where
  codec =
    dimapCodec AnnotatedSource unAnnotatedSource $
      listCodec
        ( listCodec
            ( object "Annotated" $
                (,)
                  <$> requiredField "source" "source" .= fst
                  <*> requiredField "annotation" "annotation" .= snd
            )
        )

data Covered = Covered | Uncovered | Uncoverable
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

instance HasCodec Covered where
  codec = shownBoundedEnumCodec

produceAnnotatedSource :: String -> Coverage TopLevelBinding -> AnnotatedSource
produceAnnotatedSource source coverage =
  let ls = lines source
   in AnnotatedSource $
        flip map (zip [1 ..] ls) $ \(lineNum, line) ->
          case M.lookup lineNum (produceIntervals coverage) of
            Nothing -> [(line, Uncoverable)]
            Just lineCoverage -> go 0 line (S.toAscList lineCoverage)
  where
    go :: Word -> String -> [((Word, Word), Covered)] -> [(String, Covered)]
    go _ [] _ = []
    go _ rest [] = [(rest, Uncoverable)]
    go ix s (((start, end), c) : rest) =
      let (before, afterStart) = splitAt (fromIntegral (start - ix - 1)) s
          (middle, after) = splitAt (fromIntegral (end - start)) afterStart
       in (before, Uncoverable) : (middle, c) : go end after rest

produceIntervals :: Coverage a -> Map Word (Set ((Word, Word), Covered))
produceIntervals Coverage {..} = go Covered coverageCovered $ go Uncovered coverageUncovered M.empty
  where
    go :: Covered -> Set (Coverable a) -> Map Word (Set ((Word, Word), Covered)) -> Map Word (Set ((Word, Word), Covered))

    go c s m =
      S.foldl
        ( \acc Coverable {..} ->
            case coverableLocation of
              Nothing -> acc
              Just Location {..} ->
                M.insertWith
                  S.union
                  locationLine
                  (S.singleton ((locationColumnStart, locationColumnEnd), c))
                  acc
        )
        m
        s

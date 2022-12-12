{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Dekking.Report (reportMain, computeCoverageReport, computeModuleCoverageReport) where

import Autodocodec
import Control.Arrow (second)
import Control.Monad
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as LT
import Dekking.Coverable
import Dekking.Coverage
import Dekking.OptParse
import Path
import Path.IO
import Text.Blaze.Html.Renderer.Utf8 as Blaze
import Text.Hamlet
import Text.Julius
import Text.Lucius
import Text.Printf

reportMain :: IO ()
reportMain = do
  Settings {..} <- getSettings

  coverables <- readCoverablesFiles settingCoverablesDirs
  coverage <- readCoverageFiles settingCoverageFiles

  let coverageReport = computeCoverageReport coverables coverage

  ensureDir settingOutputDir

  jsonFile <- resolveFile settingOutputDir (renderReportFile JSONFile)
  SB.writeFile (fromAbsFile jsonFile) (LB.toStrict (encodePretty coverageReport))

  styleFile <- resolveFile settingOutputDir (renderReportFile StyleFile)
  SB.writeFile (fromAbsFile styleFile) $ TE.encodeUtf8 coverageReportCss

  scriptFile <- resolveFile settingOutputDir (renderReportFile ScriptFile)
  SB.writeFile (fromAbsFile scriptFile) $ TE.encodeUtf8 coverageReportJS

  reportFile <- resolveFile settingOutputDir (renderReportFile IndexFile)
  SB.writeFile (fromAbsFile reportFile) $ LB.toStrict $ Blaze.renderHtml $ htmlCoverageReport coverageReport

  forM_ (M.toList (coverageReportModules coverageReport)) $ \(pn, m) -> do
    packagePath <- resolveFile settingOutputDir (renderReportFile (PackageFile pn))
    ensureDir (parent packagePath)
    SB.writeFile (fromAbsFile packagePath) $
      LB.toStrict $ Blaze.renderHtml $ htmlPackageCoverageReport pn m

  forM_ (concatMap (\(pn, mn) -> (,) pn <$> M.toList mn) (M.toList (coverageReportModules coverageReport))) $ \(pn, (mn, mc)) -> do
    modulePath <- resolveFile settingOutputDir (renderReportFile (ModuleFile pn mn))
    ensureDir (parent modulePath)
    SB.writeFile (fromAbsFile modulePath) $
      LB.toStrict $ Blaze.renderHtml $ htmlModuleCoverageReport pn mn mc

data ReportFile
  = JSONFile
  | StyleFile
  | ScriptFile
  | IndexFile
  | PackageFile !PackageName
  | ModuleFile !PackageName !ModuleName

renderReportFile :: ReportFile -> FilePath
renderReportFile = \case
  JSONFile -> "report.json"
  StyleFile -> "style.css"
  ScriptFile -> "script.js"
  IndexFile -> "index.html"
  PackageFile pn -> packageFileName pn
  ModuleFile pn mn -> moduleFileName pn mn

packageFileName :: PackageName -> FilePath
packageFileName pn = pn <> ".html"

moduleFileName :: PackageName -> ModuleName -> FilePath
moduleFileName pn mn = pn <> mn <> ".html"

reportUrlRender :: ReportFile -> [(Text, Text)] -> Text
reportUrlRender rf _ = T.pack $ renderReportFile rf

coverageReportCss :: Text
coverageReportCss = LT.toStrict $ renderCss $ $(luciusFile "templates/style.lucius") reportUrlRender

coverageReportJS :: Text
coverageReportJS = LT.toStrict $ renderJavascript $ $(juliusFile "templates/script.julius") reportUrlRender

htmlCoverageReport :: CoverageReport -> Html
htmlCoverageReport CoverageReport {..} =
  let unwrapped = concatMap (\(pn, ms) -> (,) pn <$> M.toList ms) (M.toList coverageReportModules)
      summaries = map (second (second (\ModuleCoverageReport {..} -> computeCoverageSummary moduleCoverageReportExpressions))) unwrapped
      totalExpressionSummary = foldMap (snd . snd) summaries
   in $(hamletFile "templates/index.hamlet") reportUrlRender

htmlPackageCoverageReport :: PackageName -> Map ModuleName ModuleCoverageReport -> Html
htmlPackageCoverageReport packageName moduleCoverageReports =
  let summaries = map (second (\ModuleCoverageReport {..} -> computeCoverageSummary moduleCoverageReportExpressions)) (M.toList moduleCoverageReports)
      totalExpressionSummary = foldMap snd summaries
   in $(hamletFile "templates/package.hamlet") reportUrlRender

htmlModuleCoverageReport :: PackageName -> ModuleName -> ModuleCoverageReport -> Html
htmlModuleCoverageReport packageName moduleName ModuleCoverageReport {..} =
  let annotatedLines = zip [(1 :: Word) ..] (unAnnotatedSource moduleCoverageReportAnnotatedSource)
      expressionSummary = computeCoverageSummary moduleCoverageReportExpressions
   in $(hamletFile "templates/module.hamlet") reportUrlRender

coveredCaseClass :: Covered -> Maybe String
coveredCaseClass = \case
  Covered -> Just coveredClass
  Uncovered -> Just uncoveredClass
  Uncoverable -> Nothing

coveredClass :: String
coveredClass = "covered"

uncoveredClass :: String
uncoveredClass = "uncovered"

coveredColour :: String
coveredColour = "#00aa00"

uncoveredColour :: String
uncoveredColour = "yellow"

computeCoverageReport :: Coverables -> Set (PackageName, ModuleName, Location) -> CoverageReport
computeCoverageReport Coverables {..} coverage =
  CoverageReport $
    M.mapWithKey
      ( \packageName modules ->
          M.mapWithKey
            ( \moduleName (sourceCode, moduleCoverables) ->
                let relevantCoverage =
                      S.fromList
                        . mapMaybe
                          ( \(pn, mm, tlb) ->
                              if pn == packageName && mm == moduleName
                                then Just tlb
                                else Nothing
                          )
                        . S.toList
                        $ coverage
                 in computeModuleCoverageReport sourceCode moduleCoverables relevantCoverage
            )
            modules
      )
      coverablesModules

newtype CoverageReport = CoverageReport {coverageReportModules :: Map PackageName (Map ModuleName ModuleCoverageReport)}
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec CoverageReport)

instance HasCodec CoverageReport where
  codec = dimapCodec CoverageReport coverageReportModules codec

computeModuleCoverageReport :: String -> ModuleCoverables -> Set Location -> ModuleCoverageReport
computeModuleCoverageReport sourceCode ModuleCoverables {..} covereds =
  let expressionCoverage = computeCoverage moduleCoverablesExpressions covereds
   in ModuleCoverageReport
        { moduleCoverageReportAnnotatedSource = produceAnnotatedSource sourceCode expressionCoverage,
          moduleCoverageReportExpressions = expressionCoverage
        }

data ModuleCoverageReport = ModuleCoverageReport
  { moduleCoverageReportAnnotatedSource :: AnnotatedSource,
    moduleCoverageReportExpressions :: Coverage Expression
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ModuleCoverageReport)

instance HasCodec ModuleCoverageReport where
  codec =
    object "ModuleCoverageReport" $
      ModuleCoverageReport
        <$> requiredField "annotated-source" "annotated source" .= moduleCoverageReportAnnotatedSource
        <*> requiredField "expressions" "expressions" .= moduleCoverageReportExpressions

data Coverage a = Coverage
  { coverageCovered :: Set (Coverable a),
    coverageUncovered :: Set (Coverable a)
  }
  deriving (Show, Eq, Ord)

instance Ord a => Semigroup (Coverage a) where
  (<>) c1 c2 =
    Coverage
      { coverageCovered = coverageCovered c1 <> coverageCovered c2,
        coverageUncovered = coverageUncovered c1 <> coverageUncovered c2
      }

instance (Ord a, HasCodec a) => HasCodec (Coverage a) where
  codec =
    object "Coverage" $
      Coverage
        <$> requiredField "covered" "covered values" .= coverageCovered
        <*> requiredField "uncovered" "uncovered values" .= coverageUncovered

computeCoverage :: Set (Coverable a) -> Set Location -> Coverage a
computeCoverage coverables covereds =
  Coverage
    { coverageCovered = S.filter ((`S.member` covereds) . coverableLocation) coverables,
      coverageUncovered = S.filter (not . (`S.member` covereds) . coverableLocation) coverables
    }

data CoverageSummary = CoverageSummary
  { coverageSummaryUncovered :: !Word,
    coverageSummaryCovered :: !Word
  }
  deriving (Show, Eq, Ord)

computeCoverageSummary :: Coverage a -> CoverageSummary
computeCoverageSummary Coverage {..} =
  let coverageSummaryUncovered = fromIntegral $ S.size coverageUncovered
      coverageSummaryCovered = fromIntegral $ S.size coverageCovered
   in CoverageSummary {..}

coverageSummaryTotal :: CoverageSummary -> Word
coverageSummaryTotal CoverageSummary {..} = coverageSummaryUncovered + coverageSummaryCovered

coverageSummaryRatio :: CoverageSummary -> Float
coverageSummaryRatio cs = fromIntegral (coverageSummaryCovered cs) / fromIntegral (coverageSummaryTotal cs)

coverageSummaryPercentage :: CoverageSummary -> String
coverageSummaryPercentage cs = printf "%.0f" (100 * coverageSummaryRatio cs)

instance Semigroup CoverageSummary where
  (<>) c1 c2 =
    CoverageSummary
      { coverageSummaryUncovered = coverageSummaryUncovered c1 + coverageSummaryUncovered c2,
        coverageSummaryCovered = coverageSummaryCovered c1 + coverageSummaryCovered c2
      }

instance Monoid CoverageSummary where
  mempty =
    CoverageSummary
      { coverageSummaryUncovered = 0,
        coverageSummaryCovered = 0
      }
  mappend = (<>)

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

produceAnnotatedSource :: String -> Coverage a -> AnnotatedSource
produceAnnotatedSource source coverage =
  let ls = lines source
   in AnnotatedSource $
        flip map (zip [1 ..] ls) $ \(lineNum, line) ->
          case M.lookup lineNum (produceIntervals coverage) of
            Nothing -> [(line, Uncoverable)]
            Just lineCoverage -> go 1 line (S.toAscList lineCoverage)
  where
    go :: Word -> String -> [((Word, Word), Covered)] -> [(String, Covered)]
    go _ [] _ = []
    go _ rest [] = [(rest, Uncoverable)]
    go ix s (((start, end), c) : rest) =
      let (before, afterStart) = splitAt (fromIntegral (start - ix)) s
          (middle, after) = splitAt (fromIntegral (end - start)) afterStart
       in (before, Uncoverable) : (middle, c) : go end after rest

produceIntervals :: Coverage a -> Map Word (Set ((Word, Word), Covered))
produceIntervals Coverage {..} = go Covered coverageCovered $ go Uncovered coverageUncovered M.empty
  where
    go :: Covered -> Set (Coverable a) -> Map Word (Set ((Word, Word), Covered)) -> Map Word (Set ((Word, Word), Covered))

    go c s m =
      S.foldl
        ( \acc Coverable {..} ->
            let Location {..} = coverableLocation
             in M.insertWith
                  S.union
                  locationLine
                  (S.singleton ((locationColumnStart, locationColumnEnd), c))
                  acc
        )
        m
        s

mkProgressBar :: CoverageSummary -> Html
mkProgressBar coverageSummary =
  let bothZero = coverageSummaryCovered coverageSummary == 0 && coverageSummaryTotal coverageSummary == 0
      val = if bothZero then 1 else coverageSummaryCovered coverageSummary
      mx = if bothZero then 1 else coverageSummaryTotal coverageSummary
   in $(hamletFile "templates/progress-bar.hamlet") (error "unused")

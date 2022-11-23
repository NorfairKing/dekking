{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain, computeCoverageReport, computeModuleCoverageReport) where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Dekking.Coverable
import Dekking.Coverage
import Dekking.OptParse
import Text.Colour
import Text.Show.Pretty

reportMain :: IO ()
reportMain = do
  Settings {..} <- getSettings

  coverables <- readCoverablesFiles settingCoverablesDirs
  coverage <- readCoverageFiles settingCoverageFiles

  pPrint coverables
  pPrint coverage
  let coverageReport = computeCoverageReport coverables coverage
  pPrint coverageReport
  putChunksUtf8With With24BitColours (concatMap (\cs -> cs ++ ["\n"]) (colouredCoverageReport coverageReport))

colouredCoverageReport :: CoverageReport -> [[Chunk]]
colouredCoverageReport CoverageReport {..} =
  flip concatMap (M.toList coverageReportModules) $ \(moduleName, ModuleCoverageReport {..}) ->
    concat
      [ [[fore blue $ chunk (T.pack moduleName)], []],
        flip map (unAnnotatedSource moduleCoverageReportAnnotatedSource) $ \ls ->
          flip map ls $ \(s, c) ->
            let withCovered = case c of
                  Uncoverable -> id
                  Uncovered -> fore red
                  Covered -> fore green
             in withCovered $ chunk (T.pack s),
        [[], []]
      ]

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

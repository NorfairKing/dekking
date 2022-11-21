{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain) where

import Data.List
import Dekking.Coverable
import Dekking.Coverage
import Path
import Path.IO
import System.Environment (getArgs)
import Text.Show.Pretty

reportMain :: IO ()
reportMain = do
  args <- getArgs :: IO [FilePath]

  dirs <- case args of
    [] -> (: []) <$> getCurrentDir
    _ -> mapM resolveDir' args

  fs <- concat <$> mapM ((fmap snd . listDirRecur) :: Path Abs Dir -> IO [Path Abs File]) (dirs :: [Path Abs Dir])

  coverage <-
    foldMap readCoverageFile $
      filter
        ((== [relfile|coverage.dat|]) . filename)
        (fs :: [Path Abs File])
  coverables <-
    foldMap readCoverableFile $
      filter
        (maybe False (isSuffixOf "coverable") . fileExtension)
        (fs :: [Path Abs File])

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

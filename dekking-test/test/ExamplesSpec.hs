module ExamplesSpec (spec) where

import qualified Data.Set as S
import Dekking.Coverable
import Dekking.Coverage
import Dekking.Report
import Examples.Multi.A
import Examples.Multi.B
import Examples.TopLevel
import Path
import Path.IO
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = sequential $ do
  describe "TopLevel" $ do
    it "Makes the same coverables for TopLevel.hs" $
      goldenStringFile
        "test_resources/Examples/TopLevel.hs.coverables"
        (readFile "src/Examples/TopLevel.hs.coverables")
    it "outputs some coverage information" $ do
      let don't _ = pure ()
      coverageFile <- resolveFile' "coverage.dat"
      ignoringAbsence $ removeFile coverageFile
      covered
      coveredWithArg 5
      don't uncovered
      don't $ uncoveredWithArg 5
      pure $ goldenStringFile "test_resources/Examples/TopLevel.hs.coverage" (readFile (fromAbsFile coverageFile))
    it "makes a coverage report" $ do
      coverableFile <- resolveFile' "test_resources/Examples/TopLevel.hs.coverables"
      coverables <- readModuleCoverablesFile coverableFile
      coverageFile <- resolveFile' "test_resources/Examples/TopLevel.hs.coverage"
      coverage <- readCoverageFile coverageFile
      pure $
        pureGoldenJSONValueFile
          "test_resources/Examples/TopLevel.hs.report"
          (computeModuleCoverageReport coverables (S.map (\(_, _, x) -> x) coverage))

  describe "Multi" $ do
    it "Makes the same coverables for the Multi modules" $ do
      dir <- resolveDir' "src/Examples/Multi" :: IO (Path Abs Dir)
      pure $
        goldenJSONValueFile
          "test_resources/Examples/Multi.coverables"
          (readCoverablesFiles (S.singleton dir))
    it "Outputs some coverage information" $ do
      let don't _ = pure ()
      coverageFile <- resolveFile' "coverage.dat"
      ignoringAbsence $ removeFile coverageFile
      five `shouldBe` 5
      don't $ six `shouldBe` 6
      pure $ goldenStringFile "test_resources/Examples/Multi.coverage" (readFile (fromAbsFile coverageFile))
    it "makes a coverage report" $ do
      dir <- resolveDir' "src/Examples/Multi" :: IO (Path Abs Dir)
      coverables <- readCoverablesFiles (S.singleton dir)
      coverageFile <- resolveFile' "test_resources/Examples/Multi.coverage"
      coverage <- readCoverageFile coverageFile
      pure $
        pureGoldenJSONValueFile
          "test_resources/Examples/Multi.report"
          (computeCoverageReport coverables coverage)

module ExamplesSpec (spec) where

import Dekking.Coverable
import Dekking.Coverage
import Dekking.Report
import Examples.TopLevel
import Path
import Path.IO
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = sequential $ do
  it "Makes the same coverables for TopLevel.hs" $
    goldenStringFile
      "test_resources/Examples/TopLevel.hs.coverables"
      (readFile "test/Examples/TopLevel.hs.coverables")
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
    coverables <- readCoverableFile coverableFile
    coverageFile <- resolveFile' "test_resources/Examples/TopLevel.hs.coverage"
    coverage <- readCoverageFile coverageFile
    pure $ pureGoldenJSONValueFile "test_resources/Examples/TopLevel.hs.report" (computeModuleCoverageReport coverables coverage)

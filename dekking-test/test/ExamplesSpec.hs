module ExamplesSpec (spec) where

import qualified Data.Set as S
import Dekking.Coverable
import Dekking.Coverage
import Dekking.Report
import Examples.Multi.A
import Examples.Multi.B
import qualified Examples.OverloadedStrings as OverloadedStrings
import qualified Examples.Paren as Paren
import qualified Examples.TopLevel as TopLevel
import Path
import Path.IO
import Test.Syd
import Test.Syd.Aeson

spec :: Spec
spec = sequential . doNotRandomiseExecutionOrder $ do
  let singleFileSpec name coveringCode = do
        describe name $ do
          let coverablesFilePath = "src/Examples/" <> name <> ".hs.coverables"
          let coverablesGoldenFilePath = "test_resources/Examples/" <> name <> ".hs.coverables"
          let coverageGoldenFilePath = "test_resources/Examples/" <> name <> ".hs.coverage"
          let reportGoldenFilePath = "test_resources/Examples/" <> name <> ".hs.report"
          it ("Makes the same coverables for " <> name <> ".hs") $
            goldenStringFile
              coverablesGoldenFilePath
              (readFile coverablesFilePath)

          it "outputs some coverage information" $ do
            coverageFile <- resolveFile' "coverage.dat"
            ignoringAbsence $ removeFile coverageFile

            coveringCode :: IO ()

            pure $ goldenStringFile coverageGoldenFilePath (readFile (fromAbsFile coverageFile))

          it "makes a coverage report" $ do
            coverablesPath <- resolveFile' coverablesGoldenFilePath
            coverablesFile <- readModuleCoverablesFile coverablesPath
            coverageFile <- resolveFile' coverageGoldenFilePath
            coverage <- readCoverageFile coverageFile
            pure $
              pureGoldenJSONValueFile
                reportGoldenFilePath
                ( computeModuleCoverageReport
                    (moduleCoverablesFileSource coverablesFile)
                    (moduleCoverablesFileCoverables coverablesFile)
                    (S.map (\(_, _, x) -> x) coverage)
                )

  let don't _ = pure ()
  singleFileSpec "TopLevel" $ do
    TopLevel.covered
    TopLevel.coveredWithArg 5
    don't TopLevel.uncovered
    don't $ TopLevel.uncoveredWithArg 5

  singleFileSpec "Paren" Paren.main
  singleFileSpec "OverloadedStrings" OverloadedStrings.main

  describe "Multi" $ do
    it "Makes the same coverables for the Multi modules" $ do
      dir <- resolveDir' "src/Examples/Multi" :: IO (Path Abs Dir)
      pure $
        goldenJSONValueFile
          "test_resources/Examples/Multi.coverables"
          (readCoverablesFiles (S.singleton dir))
    it "Outputs some coverage information" $ do
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

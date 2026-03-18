{-# LANGUAGE RecordWildCards #-}

module Dekking.Report (reportMain) where

import Control.Monad
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import qualified Data.Text.Encoding as TE
import Dekking.Coverable
import Dekking.Coverage
import Dekking.OptParse
import Dekking.Report.Html
import Dekking.Report.Uncovered
import Path
import Path.IO
import Text.Blaze.Html.Renderer.Utf8 as Blaze

reportMain :: IO ()
reportMain = do
  Settings {..} <- getSettings

  coverables <- readCoverablesFiles settingCoverablesDirs
  coverage <- readCoverageFiles settingCoverageFiles

  let coverageReport = computeCoverageReport coverables coverage
      uncoveredReport = computeUncoveredReport coverageReport

  ensureDir settingOutputDir

  jsonFile <- resolveFile settingOutputDir (renderReportFile JSONFile)
  SB.writeFile (fromAbsFile jsonFile) (LB.toStrict (encodePretty coverageReport))

  uncoveredJsonFile <- resolveFile settingOutputDir (renderReportFile UncoveredJSONFile)
  SB.writeFile (fromAbsFile uncoveredJsonFile) (LB.toStrict (encodePretty uncoveredReport))

  uncoveredTextFile <- resolveFile settingOutputDir (renderReportFile UncoveredTextFile)
  SB.writeFile (fromAbsFile uncoveredTextFile) (TE.encodeUtf8 (renderUncoveredText uncoveredReport))

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
      LB.toStrict $
        Blaze.renderHtml $
          htmlPackageCoverageReport pn m

  forM_ (concatMap (\(pn, mn) -> (,) pn <$> M.toList mn) (M.toList (coverageReportModules coverageReport))) $ \(pn, (mn, mc)) -> do
    modulePath <- resolveFile settingOutputDir (renderReportFile (ModuleFile pn mn))
    ensureDir (parent modulePath)
    SB.writeFile (fromAbsFile modulePath) $
      LB.toStrict $
        Blaze.renderHtml $
          htmlModuleCoverageReport pn mn mc

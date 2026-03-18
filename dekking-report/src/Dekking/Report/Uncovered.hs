{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.Report.Uncovered
  ( UncoveredReport (..),
    UncoveredPackage (..),
    UncoveredModule (..),
    UncoveredBinding (..),
    UncoveredExpression (..),
    computeUncoveredReport,
    renderUncoveredText,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.List (intersperse, sortOn)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Dekking.Coverable
import Dekking.Report.Html

data UncoveredReport = UncoveredReport
  { uncoveredReportPackages :: [UncoveredPackage]
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec UncoveredReport)

instance HasCodec UncoveredReport where
  codec =
    object "UncoveredReport" $
      UncoveredReport
        <$> requiredField "packages" "packages with uncovered expressions" .= uncoveredReportPackages

data UncoveredPackage = UncoveredPackage
  { uncoveredPackageName :: PackageName,
    uncoveredPackageModules :: [UncoveredModule]
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec UncoveredPackage)

instance HasCodec UncoveredPackage where
  codec =
    object "UncoveredPackage" $
      UncoveredPackage
        <$> requiredField "package" "package name" .= uncoveredPackageName
        <*> requiredField "modules" "modules with uncovered expressions" .= uncoveredPackageModules

data UncoveredModule = UncoveredModule
  { uncoveredModuleName :: ModuleName,
    uncoveredModuleCovered :: Word,
    uncoveredModuleTotal :: Word,
    uncoveredModuleBindings :: [UncoveredBinding]
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec UncoveredModule)

instance HasCodec UncoveredModule where
  codec =
    object "UncoveredModule" $
      UncoveredModule
        <$> requiredField "module" "module name" .= uncoveredModuleName
        <*> requiredField "covered" "number of covered expressions" .= uncoveredModuleCovered
        <*> requiredField "total" "total number of expressions" .= uncoveredModuleTotal
        <*> requiredField "bindings" "uncovered top-level bindings" .= uncoveredModuleBindings

data UncoveredBinding = UncoveredBinding
  { uncoveredBindingName :: Maybe String,
    uncoveredBindingExpressions :: [UncoveredExpression]
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec UncoveredBinding)

instance HasCodec UncoveredBinding where
  codec =
    object "UncoveredBinding" $
      UncoveredBinding
        <$> requiredField "binding" "top-level binding name" .= uncoveredBindingName
        <*> requiredField "expressions" "uncovered expressions" .= uncoveredBindingExpressions

data UncoveredExpression = UncoveredExpression
  { uncoveredExpressionLine :: Word,
    uncoveredExpressionColumnStart :: Word,
    uncoveredExpressionColumnEnd :: Word,
    uncoveredExpressionIdentifier :: String
  }
  deriving (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec UncoveredExpression)

instance HasCodec UncoveredExpression where
  codec =
    object "UncoveredExpression" $
      UncoveredExpression
        <$> requiredField "line" "line number" .= uncoveredExpressionLine
        <*> requiredField "column-start" "start column" .= uncoveredExpressionColumnStart
        <*> requiredField "column-end" "end column" .= uncoveredExpressionColumnEnd
        <*> requiredField "identifier" "expression identifier" .= uncoveredExpressionIdentifier

computeUncoveredReport :: CoverageReport -> UncoveredReport
computeUncoveredReport CoverageReport {..} =
  UncoveredReport
    { uncoveredReportPackages =
        map
          ( \(pn, modules) ->
              UncoveredPackage
                { uncoveredPackageName = pn,
                  uncoveredPackageModules =
                    map
                      ( \(mn, ModuleCoverageReport {..}) ->
                          let summary = computeCoverageSummary moduleCoverageReportExpressions
                              namedUncovered =
                                mapMaybe
                                  ( \Coverable {..} ->
                                      case expressionIdentifier coverableValue of
                                        Nothing -> Nothing
                                        Just ident ->
                                          let Location {..} = coverableLocation
                                           in Just
                                                ( expressionTopLevelBinding coverableValue,
                                                  UncoveredExpression
                                                    { uncoveredExpressionLine = locationLine,
                                                      uncoveredExpressionColumnStart = locationColumnStart,
                                                      uncoveredExpressionColumnEnd = locationColumnEnd,
                                                      uncoveredExpressionIdentifier = ident
                                                    }
                                                )
                                  )
                                  . S.toList
                                  $ coverageUncovered moduleCoverageReportExpressions
                              grouped =
                                M.fromListWith
                                  (<>)
                                  [(b, [e]) | (b, e) <- namedUncovered]
                              bindings =
                                sortOn (minimumOr maxBound . map uncoveredExpressionLine . uncoveredBindingExpressions)
                                  . map
                                    ( \(b, es) ->
                                        UncoveredBinding
                                          { uncoveredBindingName = b,
                                            uncoveredBindingExpressions = sortOn uncoveredExpressionLine es
                                          }
                                    )
                                  . M.toList
                                  $ grouped
                           in UncoveredModule
                                { uncoveredModuleName = mn,
                                  uncoveredModuleCovered = coverageSummaryCovered summary,
                                  uncoveredModuleTotal = coverageSummaryTotal summary,
                                  uncoveredModuleBindings = bindings
                                }
                      )
                      (M.toList modules)
                }
          )
          (M.toList coverageReportModules)
    }

renderUncoveredText :: UncoveredReport -> Text
renderUncoveredText report =
  TL.toStrict . TB.toLazyText $ renderUncoveredBuilder report

renderUncoveredBuilder :: UncoveredReport -> Builder
renderUncoveredBuilder UncoveredReport {..} =
  preamble
    <> foldMap renderPackage uncoveredReportPackages
  where
    preamble :: Builder
    preamble =
      "# Uncovered expressions report\n\
      \\n\
      \The following report lists all uncovered expressions.\n\
      \It is structured as follows:\n\
      \\n\
      \Module headers: # <package> <module> <covered>/<total>\n\
      \Each subsequent line: <first-line> <binding>: <identifier1>, <identifier2>, ...\n\
      \\n\
      \\n"

    renderPackage :: UncoveredPackage -> Builder
    renderPackage UncoveredPackage {..} =
      foldMap (renderModule uncoveredPackageName) uncoveredPackageModules

    renderModule :: PackageName -> UncoveredModule -> Builder
    renderModule pn UncoveredModule {..} =
      case uncoveredModuleBindings of
        [] -> mempty
        bs ->
          "# "
            <> TB.fromString pn
            <> " "
            <> TB.fromString uncoveredModuleName
            <> " "
            <> TB.fromString (show uncoveredModuleCovered)
            <> "/"
            <> TB.fromString (show uncoveredModuleTotal)
            <> "\n"
            <> foldMap (\b -> renderBinding b <> "\n") bs

    renderBinding :: UncoveredBinding -> Builder
    renderBinding UncoveredBinding {..} =
      let minLine = minimumOr 0 $ map uncoveredExpressionLine uncoveredBindingExpressions
          idents = S.toAscList . S.fromList $ map uncoveredExpressionIdentifier uncoveredBindingExpressions
       in TB.fromString (show minLine)
            <> " "
            <> TB.fromString (fromMaybe "(unknown)" uncoveredBindingName)
            <> ": "
            <> mconcat (intersperse ", " (map TB.fromString idents))

minimumOr :: (Ord a) => a -> [a] -> a
minimumOr def [] = def
minimumOr _ xs = minimum xs

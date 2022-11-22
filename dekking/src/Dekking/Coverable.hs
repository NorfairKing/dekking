{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.Coverable where

import Autodocodec
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Path
import Path.IO
import Text.Show.Pretty (pPrint)

newtype Coverables = Coverables {coverablesModules :: Map ModuleName ModuleCoverables}
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Coverables)

instance Semigroup Coverables where
  (<>) c1 c2 =
    Coverables
      { coverablesModules =
          coverablesModules c1 Prelude.<> coverablesModules c2
      }

instance Monoid Coverables where
  mempty = Coverables {coverablesModules = mempty}
  mappend = (Prelude.<>)

instance HasCodec Coverables where
  codec = dimapCodec Coverables coverablesModules codec

data ModuleCoverables = ModuleCoverables
  { moduleCoverablesModuleName :: ModuleName,
    moduleCoverablesTopLevelBindings :: Set (Coverable TopLevelBinding)
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ModuleCoverables)

instance HasCodec ModuleCoverables where
  codec =
    object "ModuleCoverables" $
      ModuleCoverables
        <$> requiredField "module-name" "Module name" .= moduleCoverablesModuleName
        <*> optionalFieldWithOmittedDefault "top-level-bindings" mempty "Top level bindings" .= moduleCoverablesTopLevelBindings

data Coverable a = Coverable
  { coverableValue :: a,
    coverableLocation :: Maybe Location
  }
  deriving stock (Show, Eq, Ord)

instance HasCodec a => HasCodec (Coverable a) where
  codec =
    object "Coverable" $
      Coverable
        <$> requiredField "value" "the value to be covered" .= coverableValue
        <*> optionalField "location" "the location of the value to be covered" .= coverableLocation

data Location = Location
  { locationLine :: Word,
    locationColumnStart :: Word,
    locationColumnEnd :: Word
  }
  deriving stock (Show, Eq, Ord)

instance HasCodec Location where
  codec =
    object "Location" $
      Location
        <$> requiredField "line" "the line number" .= locationLine
        <*> requiredField "start" "the start column" .= locationColumnStart
        <*> requiredField "end" "the end column" .= locationColumnEnd

newtype TopLevelBinding = TopLevelBinding {topLevelBindingIdentifier :: String}
  deriving stock (Show, Eq, Ord)

instance HasCodec TopLevelBinding where
  codec = dimapCodec TopLevelBinding topLevelBindingIdentifier codec

type ModuleName = String

readModuleCoverablesFile :: Path Abs File -> IO ModuleCoverables
readModuleCoverablesFile p = do
  errOrRes <- eitherDecodeFileStrict (fromAbsFile p)
  case errOrRes of
    Left err -> fail err
    Right result -> pure result

writeModuleCoverablesFile :: Path Abs File -> ModuleCoverables -> IO ()
writeModuleCoverablesFile p moduleCoverables = do
  SB.writeFile (fromAbsFile p) (LB.toStrict (encodePretty moduleCoverables))

readCoverablesFiles :: Set (Path Abs Dir) -> IO Coverables
readCoverablesFiles dirs = do
  coverablesFiles <-
    filter
      (maybe False (isSuffixOf coverablesExtension) . fileExtension)
      . concat
      <$> mapM (fmap snd . listDirRecur) (S.toList dirs)
  fmap (Coverables . M.fromList) $
    forM coverablesFiles $ \coverablesFile -> do
      print coverablesFile
      coverables <- readModuleCoverablesFile coverablesFile
      pPrint coverables
      pure (moduleCoverablesModuleName coverables, coverables)

coverablesExtension :: String
coverablesExtension = ".coverables"

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.Coverable where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Map (Map)
import Data.Set (Set)
import Path

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
  { moduleCoverablesTopLevelBindings :: Set (Coverable TopLevelBinding)
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec ModuleCoverables)

instance Semigroup ModuleCoverables where
  (<>) c1 c2 =
    ModuleCoverables
      { moduleCoverablesTopLevelBindings =
          moduleCoverablesTopLevelBindings c1 Prelude.<> moduleCoverablesTopLevelBindings c2
      }

instance Monoid ModuleCoverables where
  mempty = ModuleCoverables {moduleCoverablesTopLevelBindings = mempty}
  mappend = (Prelude.<>)

instance HasCodec ModuleCoverables where
  codec =
    object "ModuleCoverables" $
      ModuleCoverables
        <$> optionalFieldWithOmittedDefault "top-level-bindings" mempty "Top level bindings" .= moduleCoverablesTopLevelBindings

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

data TopLevelBinding = TopLevelBinding
  { topLevelBindingModuleName :: Maybe ModuleName,
    topLevelBindingIdentifier :: String
  }
  deriving stock (Show, Eq, Ord)

instance HasCodec TopLevelBinding where
  codec =
    object "TopLevelBinding" $
      TopLevelBinding
        <$> optionalField "module" "the module in which this binding was found" .= topLevelBindingModuleName
        <*> requiredField "identifier" "the identifier of the top level binding" .= topLevelBindingIdentifier

type ModuleName = String

readModuleCoverablesFile :: Path Abs File -> IO ModuleCoverables
readModuleCoverablesFile p = do
  errOrRes <- eitherDecodeFileStrict (fromAbsFile p)
  case errOrRes of
    Left err -> fail err
    Right result -> pure result

coverablesExtension :: String
coverablesExtension = "coverables"

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.Coverable where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Data.Set (Set)
import Path

data Coverables = Coverables
  { coverableTopLevelBindings :: Set (Coverable TopLevelBinding)
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Coverables)

instance Semigroup Coverables where
  (<>) c1 c2 =
    Coverables
      { coverableTopLevelBindings =
          coverableTopLevelBindings c1 Prelude.<> coverableTopLevelBindings c2
      }

instance Monoid Coverables where
  mempty = Coverables {coverableTopLevelBindings = mempty}
  mappend = (Prelude.<>)

instance HasCodec Coverables where
  codec =
    object "Coverables" $
      Coverables
        <$> optionalFieldWithOmittedDefault "top-level-bindings" mempty "Top level bindings" .= coverableTopLevelBindings

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

readCoverableFile :: Path Abs File -> IO Coverables
readCoverableFile p = do
  errOrRes <- eitherDecodeFileStrict (fromAbsFile p)
  case errOrRes of
    Left err -> fail err
    Right result -> pure result

coverablesExtension :: String
coverablesExtension = "coverables"

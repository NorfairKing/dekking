{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.Coverable where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict)
import Path

data Coverables = Coverables
  { coverableTopLevelBindings :: [CoverableTopLevelBinding]
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Coverables)

instance Semigroup Coverables where
  (<>) c1 c2 = Coverables {coverableTopLevelBindings = coverableTopLevelBindings c1 Prelude.<> coverableTopLevelBindings c2}

instance Monoid Coverables where
  mempty = Coverables {coverableTopLevelBindings = mempty}
  mappend = (Prelude.<>)

instance HasCodec Coverables where
  codec =
    object "Coverables" $
      Coverables
        <$> optionalFieldWithOmittedDefault "top-level-bindings" mempty "Top level bindings" .= coverableTopLevelBindings

type CoverableTopLevelBinding = String

readCoverableFile :: Path Abs File -> IO Coverables
readCoverableFile p = do
  print p
  errOrRes <- eitherDecodeFileStrict (fromAbsFile p)
  case errOrRes of
    Left err -> fail err
    Right result -> pure result

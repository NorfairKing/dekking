{-# LANGUAGE LambdaCase #-}

module Dekking.Coverage where

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Dekking.Coverable
import Path

readCoverageFile :: Path Abs File -> IO (Set (Maybe ModuleName, TopLevelBinding))
readCoverageFile p = S.fromList . mapMaybe parseIdentifier . lines <$> readFile (fromAbsFile p)

parseIdentifier :: String -> Maybe (Maybe ModuleName, TopLevelBinding)
parseIdentifier s =
  case reverse
    ( words
        ( map
            ( \case
                '.' -> ' '
                c -> c
            )
            s
        )
    ) of
    [] -> Nothing
    [x] ->
      Just (Nothing, TopLevelBinding {topLevelBindingIdentifier = x})
    (x : rest) ->
      Just (Just $ intercalate "." (reverse rest), TopLevelBinding {topLevelBindingIdentifier = x})

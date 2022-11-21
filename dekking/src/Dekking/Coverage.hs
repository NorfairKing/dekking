{-# LANGUAGE LambdaCase #-}

module Dekking.Coverage where

import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Dekking.Coverable
import Path

readCoverageFile :: Path Abs File -> IO (Set TopLevelBinding)
readCoverageFile p = S.fromList . mapMaybe parseIdentifier . lines <$> readFile (fromAbsFile p)

parseIdentifier :: String -> Maybe TopLevelBinding
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
      Just
        TopLevelBinding
          { topLevelBindingModuleName = Nothing,
            topLevelBindingIdentifier = x
          }
    (x : rest) ->
      Just
        TopLevelBinding
          { topLevelBindingModuleName = Just $ intercalate "." (reverse rest),
            topLevelBindingIdentifier = x
          }

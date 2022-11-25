module Dekking.Coverage where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Dekking.Coverable
import Path

readCoverageFiles :: Set (Path Abs File) -> IO (Set (Maybe PackageName, Maybe ModuleName, TopLevelBinding))
readCoverageFiles = foldMap (\f -> print f >> readCoverageFile f)

readCoverageFile :: Path Abs File -> IO (Set (Maybe PackageName, Maybe ModuleName, TopLevelBinding))
readCoverageFile p = S.fromList . mapMaybe parseIdentifier . lines <$> readFile (fromAbsFile p)

parseIdentifier :: String -> Maybe (Maybe PackageName, Maybe ModuleName, TopLevelBinding)
parseIdentifier s =
  case words s of
    [] -> Nothing
    [x, y, z] ->
      Just (Just x, Just y, TopLevelBinding {topLevelBindingIdentifier = z})
    _ -> Nothing

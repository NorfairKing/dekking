module Dekking.Coverage where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Dekking.Coverable
import Path
import Text.Read

readCoverageFiles :: Set (Path Abs File) -> IO (Set (PackageName, ModuleName, Location))
readCoverageFiles = foldMap (\f -> print f >> readCoverageFile f)

readCoverageFile :: Path Abs File -> IO (Set (PackageName, ModuleName, Location))
readCoverageFile p = S.fromList . mapMaybe parseIdentifier . lines <$> readFile (fromAbsFile p)

parseIdentifier :: String -> Maybe (PackageName, ModuleName, Location)
parseIdentifier str =
  case words str of
    [] -> Nothing
    [x, y, ls, ss, es] -> do
      l <- readMaybe ls
      s <- readMaybe ss
      e <- readMaybe es
      Just (x, y, Location {locationLine = l, locationColumnStart = s, locationColumnEnd = e})
    _ -> Nothing

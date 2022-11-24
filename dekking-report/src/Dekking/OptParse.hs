{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Dekking.OptParse
  ( getSettings,
    Settings (..),
  )
where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as S
import GHC.Generics (Generic)
import Options.Applicative as OptParse
import Path
import Path.IO

getSettings :: IO Settings
getSettings = getFlags >>= combineToSettings

data Settings = Settings
  { settingCoverablesDirs :: Set (Path Abs Dir),
    settingCoverageFiles :: Set (Path Abs File)
  }
  deriving (Show, Eq, Generic)

combineToSettings :: Flags -> IO Settings
combineToSettings Flags {..} = do
  settingCoverablesDirs <- S.fromList <$> mapM resolveDir' flagCoverablesDirs
  settingCoverageFiles <- S.fromList <$> mapM resolveFile' flagCoverageFiles
  pure Settings {..}

getFlags :: IO Flags
getFlags = customExecParser prefs_ parseFlags

prefs_ :: OptParse.ParserPrefs
prefs_ =
  OptParse.defaultPrefs
    { OptParse.prefShowHelpOnError = True,
      OptParse.prefShowHelpOnEmpty = True
    }

data Flags = Flags
  { flagCoverablesDirs :: ![FilePath],
    flagCoverageFiles :: ![FilePath]
  }
  deriving (Show, Eq, Generic)

parseFlags :: OptParse.ParserInfo Flags
parseFlags = OptParse.info parser modifier
  where
    modifier = OptParse.fullDesc <> OptParse.progDesc "Fill a template"
    parser =
      Flags
        <$> many
          ( strOption
              ( mconcat
                  [ long "coverables",
                    help "A directory with coverables",
                    metavar "DIRECTORY",
                    completer $ bashCompleter "directory"
                  ]
              )
          )
        <*> many
          ( strOption
              ( mconcat
                  [ long "coverage",
                    help "A coverage file",
                    metavar "FILE",
                    completer $ bashCompleter "file"
                  ]
              )
          )

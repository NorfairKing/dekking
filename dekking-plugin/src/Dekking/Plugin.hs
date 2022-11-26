module Dekking.Plugin (plugin) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.List (isPrefixOf)
import Dekking.Coverable
import Dekking.SourceAdapter
import GHC
import GHC.Driver.Plugins
import GHC.Driver.Types
import Path
import Path.IO

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = adaptParseResult}

adaptParseResult :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
adaptParseResult _ ms pr = do
  liftIO $ putStrLn "Activating the coverage logger plugin"
  let m = ms_mod ms
  let mn = moduleName m
  if "Paths_" `isPrefixOf` moduleNameString mn
    then pure pr
    else do
      -- Transform the source
      (lm', coverables) <- runReaderT (runWriterT (adaptLocatedHsModule (hpm_module pr))) m
      forM_ (ml_hs_file (ms_location ms)) $ \sourceFile ->
        -- Output the coverables
        liftIO $ do
          p <- resolveFile' sourceFile
          sourceCode <- readFile sourceFile
          coverablesFile <- addExtension coverablesExtension p
          putStrLn $
            unwords
              [ "Outputing coverables file",
                fromAbsFile coverablesFile,
                "for source file",
                fromAbsFile p
              ]
          writeModuleCoverablesFile coverablesFile $
            ModuleCoverablesFile
              { moduleCoverablesFilePackageName = unitToString (moduleUnit m),
                moduleCoverablesFileModuleName = moduleNameString mn,
                moduleCoverablesFileSource = sourceCode,
                moduleCoverablesFileCoverables = coverables
              }
      pure pr {hpm_module = lm'}

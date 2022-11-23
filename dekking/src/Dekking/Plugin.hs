module Dekking.Plugin (plugin) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
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
  let mn = moduleName (ms_mod ms)
  -- Transform the source
  (lm', bindings) <- runWriterT (adaptLocatedHsModule mn (hpm_module pr))
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
        ModuleCoverables
          { moduleCoverablesModuleName = moduleNameString mn,
            moduleCoverablesSource = sourceCode,
            moduleCoverablesTopLevelBindings = bindings
          }
  pure pr {hpm_module = lm'}

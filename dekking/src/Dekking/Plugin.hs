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
  -- Transform the source
  (lm', bindings) <- runWriterT (adaptLocatedHsModule (hpm_module pr))
  forM_ (ml_hs_file (ms_location ms)) $ \sourceFile ->
    -- Output the coverables
    liftIO $ do
      p <- resolveFile' sourceFile
      coverablesFile <- addExtension coverablesExtension p
      writeModuleCoverablesFile coverablesFile $
        ModuleCoverables
          { moduleCoverablesModuleName = moduleNameString (moduleName (ms_mod ms)),
            moduleCoverablesTopLevelBindings = bindings
          }
  pure pr {hpm_module = lm'}

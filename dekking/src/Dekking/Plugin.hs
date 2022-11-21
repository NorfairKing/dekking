module Dekking.Plugin (plugin) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import Dekking.Coverable (coverablesExtension)
import Dekking.SourceAdapter
import GHC
import GHC.Driver.Plugins
import GHC.Driver.Types

plugin :: Plugin
plugin = defaultPlugin {parsedResultAction = adaptParseResult}

adaptParseResult :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
adaptParseResult _ ms pr = do
  liftIO $ putStrLn "Activating the coverage logger plugin"
  -- Transform the source
  (lm', coverables) <- runWriterT (adaptLocatedHsModule (hpm_module pr))
  forM_ (ml_hs_file (ms_location ms)) $ \sourceFile -> do
    let coverableFileName :: FilePath
        coverableFileName = sourceFile ++ "." ++ coverablesExtension
    -- Output the coverables
    liftIO $ SB.writeFile coverableFileName (LB.toStrict (encodePretty coverables))
  pure pr {hpm_module = lm'}

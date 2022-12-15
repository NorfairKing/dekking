module Dekking.Plugin (plugin) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import Dekking.Coverable
import Dekking.SourceAdapter
import GHC
import GHC.Driver.Plugins
import GHC.Driver.Session
import GHC.Driver.Types
import GHC.LanguageExtensions
import GHC.Tc.Types
import GHC.Types.SrcLoc
import Path
import Path.IO

plugin :: Plugin
plugin =
  defaultPlugin
    { dynflagsPlugin = \_ dynFlags ->
        -- In order to perform the source-to-source transformation, we have to set 'ImpredicativeTypes'.
        --
        -- For the purposes of this explanation, our sourc-transformation might
        -- as well be `a` -> `id a`.
        -- One would think (or at least I certainly did), that this would turn
        -- any piece of code that type-checksinto something that also
        -- type-checks.
        -- However, without 'ImpredicativeTypes', it doesn't.
        --
        -- Indeed, without 'ImpredicativeTypes', this type-checks:
        --
        -- ```
        -- exampleStringL :: Lens' Example String
        -- exampleStringL = lens exampleString (\e s -> e {exampleString = s})
        -- ```
        --
        -- But this doesn't:
        --
        -- ```
        -- exampleStringL :: Lens' Example String
        -- exampleStringL = (id lens) exampleString (\e s -> e {exampleString = s})
        -- ```
        --
        -- For a simpler example, consider the following piece of code:
        -- (Thank you @lnnf107 on twitter!)
        --
        -- ```
        -- f :: Int -> (forall a. a -> a)
        -- ```
        --
        -- Our transformation would turn `f` into `id f`, but then GHC would try
        -- to instantiate the type-parameter of `id` with the polytype `Int ->
        -- (forall a. a -> a)`, which is only possible with ImpredicativeTypes.
        pure (xopt_set dynFlags ImpredicativeTypes),
      parsedResultAction = \_ ms pm -> do
        liftIO $
          putStrLn $
            unwords
              [ "Adding",
                moduleNameString adapterModuleName,
                "to the imports of",
                moduleNameString (moduleName (ms_mod ms))
              ]
        m' <- liftL (\m -> pure m {hsmodImports = adapterImport : hsmodImports m}) (hpm_module pm)
        pure $ pm {hpm_module = m'},
      typeCheckResultAction = adaptTypeCheckResult
    }

adapterImport :: LImportDecl GhcPs
adapterImport = noLoc (simpleImportDecl adapterModuleName)

adapterModuleName :: GHC.ModuleName
adapterModuleName = mkModuleName "Dekking.ValueLevelAdapter"

adaptTypeCheckResult :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
adaptTypeCheckResult es ms tcg = do
  let m = ms_mod ms
  let mn = moduleName m
  let exceptionModules = mapMaybe (stripPrefix "--exception=") es
  if "Paths_" `isPrefixOf` moduleNameString mn || moduleNameString mn `elem` exceptionModules
    then pure tcg
    else do
      liftIO $ putStrLn "Activating the coverage logger plugin"
      -- Transform the source
      (tcg', coverables) <- runReaderT (runWriterT (adaptTypeCheckedModule tcg)) (tcg_mod tcg)
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
      pure tcg'

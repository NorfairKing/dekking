{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.SourceAdapter (adaptLocatedHsModule, unitToString) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (children, transformM)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Dekking.Coverable
import GHC hiding (moduleName)
import GHC.Driver.Types as GHC
import GHC.Plugins as GHC

addCoverableTopLevelBinding :: Coverable TopLevelBinding -> AdaptM ()
addCoverableTopLevelBinding a = tell (mempty {moduleCoverablesTopLevelBindings = S.singleton a})

type AdaptM = WriterT ModuleCoverables Hsc

adapterImport :: LImportDecl GhcPs
adapterImport = noLoc (simpleImportDecl adapterModuleName)

adapterModuleName :: GHC.ModuleName
adapterModuleName = mkModuleName "Dekking.ValueLevelAdapter"

adaptLocatedHsModule :: GHC.Module -> Located HsModule -> AdaptM (Located HsModule)
adaptLocatedHsModule moduule = liftL (adaptHsModule moduule)

adaptHsModule :: GHC.Module -> HsModule -> AdaptM HsModule
adaptHsModule moduule m = do
  liftIO $ putStrLn $ "Adapting module: " ++ moduleNameString (moduleName moduule)
  decls' <- concat <$> mapM (adaptLocatedTopLevelDecl moduule) (hsmodDecls m)
  pure (m {hsmodDecls = decls', hsmodImports = adapterImport : hsmodImports m})

adaptLocatedTopLevelDecl :: GHC.Module -> Located (HsDecl GhcPs) -> AdaptM [Located (HsDecl GhcPs)]
adaptLocatedTopLevelDecl moduule lDecl = do
  lDecls' <- adaptTopLevelDecl moduule (unLoc lDecl)
  case lDecls' of
    [] -> error "must not happen, otherwise we're deleting decls."
    [x] -> do
      x' <- adaptDecl moduule x
      pure [L (getLoc lDecl) x'] -- Nothing was adapted at the top level
    [decl, modifiedDecl] -> do
      modifiedDecl' <- adaptDecl moduule modifiedDecl
      pure [L (getLoc lDecl) decl, noLoc modifiedDecl']
    _ -> error "must not happen either, otherwise we're making too many extra decls"

adaptDecl :: GHC.Module -> HsDecl GhcPs -> AdaptM (HsDecl GhcPs)
adaptDecl moduule = \case
  ValD x bind -> ValD x <$> adaptBind moduule bind
  -- TODO
  d -> pure d

adaptBind :: GHC.Module -> HsBind GhcPs -> AdaptM (HsBind GhcPs)
adaptBind moduule = \case
  FunBind x name matchGroup ticks -> FunBind x name <$> adaptMatchGroup moduule matchGroup <*> pure ticks
  -- TODO
  b -> pure b

adaptMatchGroup :: GHC.Module -> MatchGroup GhcPs (LHsExpr GhcPs) -> AdaptM (MatchGroup GhcPs (LHsExpr GhcPs))
adaptMatchGroup moduule = \case
  MG x as origin -> MG x <$> liftL (mapM (adaptLMatch moduule)) as <*> pure origin
  mg -> pure mg

adaptLMatch :: GHC.Module -> LMatch GhcPs (LHsExpr GhcPs) -> AdaptM (LMatch GhcPs (LHsExpr GhcPs))
adaptLMatch moduule = liftL (adaptMatch moduule)

adaptMatch :: GHC.Module -> Match GhcPs (LHsExpr GhcPs) -> AdaptM (Match GhcPs (LHsExpr GhcPs))
adaptMatch moduule = \case
  Match x ctx pats body -> Match x ctx pats <$> adaptGRHSs moduule body
  m -> pure m

adaptGRHSs :: GHC.Module -> GRHSs GhcPs (LHsExpr GhcPs) -> AdaptM (GRHSs GhcPs (LHsExpr GhcPs))
adaptGRHSs moduule = \case
  GRHSs x rhs localBinds -> GRHSs x <$> mapM (adaptLGRHS moduule) rhs <*> adaptLocalBinds moduule localBinds
  g -> pure g

adaptLGRHS ::
  GHC.Module ->
  LGRHS GhcPs (LHsExpr GhcPs) ->
  AdaptM (LGRHS GhcPs (LHsExpr GhcPs))
adaptLGRHS moduule = liftL (adaptGRHS moduule)

adaptGRHS :: GHC.Module -> GRHS GhcPs (LHsExpr GhcPs) -> AdaptM (GRHS GhcPs (LHsExpr GhcPs))
adaptGRHS moduule = \case
  GRHS x guards body -> GRHS x guards <$> adaptLExpr moduule body
  grhs -> pure grhs

adaptLocalBinds :: GHC.Module -> LHsLocalBinds GhcPs -> AdaptM (LHsLocalBinds GhcPs)
adaptLocalBinds moduule = \case
  -- TODO
  lbs -> pure lbs

adaptLExpr :: GHC.Module -> LHsExpr GhcPs -> AdaptM (LHsExpr GhcPs)
adaptLExpr moduule = transformM $ \le@(L span e) -> do
  if null (children le) -- is a child itself
    then case spanLocation span of
      Just loc -> pure $ L span $ applyAdapterExpr moduule loc e
      Nothing -> pure le
    else pure le

adaptTopLevelDecl :: GHC.Module -> HsDecl GhcPs -> AdaptM [HsDecl GhcPs]
adaptTopLevelDecl moduule = \case
  ValD x bind -> fmap (ValD x) <$> adaptTopLevelBind moduule bind
  SigD x sig -> (: []) . SigD x <$> duplicateTopLevelSig moduule sig
  d -> pure [d]

duplicateTopLevelSig :: GHC.Module -> Sig GhcPs -> AdaptM (Sig GhcPs)
duplicateTopLevelSig moduule = \case
  TypeSig x ls typ -> do
    let nameStrings = map (rdrNameToString (moduleUnit moduule) . unLoc) ls
    liftIO $ putStrLn $ "Duplicating type-signatures for: " ++ show nameStrings
    ls' <- fmap concat $
      forM ls $ \l -> do
        l' <- noLoc <$> adaptTopLevelName (unLoc l)
        pure [l, l']
    pure (TypeSig x ls' typ)
  sig -> pure sig

adaptTopLevelName :: RdrName -> AdaptM RdrName
adaptTopLevelName = \case
  Unqual on -> Unqual <$> adaptTopLevelOccName on
  Qual mn on -> Qual mn <$> adaptTopLevelOccName on
  Orig m on -> Orig m <$> adaptTopLevelOccName on
  Exact n -> Exact <$> adaptTopLevelExactName n

adaptTopLevelOccName :: OccName -> AdaptM OccName
adaptTopLevelOccName on = do
  let fs = occNameFS on
  let ns = occNameSpace on
  pure $ mkOccNameFS ns (fs `appendFS` "UnlikelyToCollideForCoverageXYZPoopyHead")

adaptTopLevelExactName :: Name -> AdaptM Name
adaptTopLevelExactName = undefined

adaptTopLevelBind :: GHC.Module -> HsBind GhcPs -> AdaptM [HsBind GhcPs]
adaptTopLevelBind moduule = \case
  FunBind x originalName originalMatches originalTicks -> do
    let on = rdrNameOcc (unLoc originalName)
        nameString = occNameString on
    liftIO $ putStrLn $ "Adapting bind: " ++ nameString
    let strToLog = rdrNameToString (moduleUnit moduule) (mkRdrQual (moduleName moduule) on)
    let mLoc = spanLocation (getLoc originalName)
    case mLoc of
      Nothing -> pure [FunBind x originalName originalMatches originalTicks]
      Just loc -> do
        addCoverableTopLevelBinding
          Coverable
            { coverableValue = TopLevelBinding {topLevelBindingIdentifier = nameString},
              coverableLocation = loc
            }
        adaptedName <- noLoc <$> adaptTopLevelName (unLoc originalName)
        let adaptedMatches =
              MG
                { mg_ext = NoExtField,
                  mg_alts =
                    noLoc
                      [ noLoc
                          ( Match
                              { m_ext = NoExtField,
                                m_ctxt =
                                  FunRhs
                                    { mc_fun = adaptedName,
                                      mc_fixity = Prefix,
                                      mc_strictness = NoSrcStrict
                                    },
                                m_pats = [],
                                m_grhss =
                                  GRHSs
                                    { grhssExt = NoExtField,
                                      grhssGRHSs =
                                        [ noLoc
                                            ( GRHS
                                                NoExtField
                                                []
                                                (noLoc (applyAdapterExpr moduule loc (HsVar NoExtField adaptedName)))
                                            )
                                        ],
                                      grhssLocalBinds = noLoc (EmptyLocalBinds NoExtField)
                                    }
                              }
                          )
                      ],
                  mg_origin = Generated
                }
        pure
          [ -- The original function will now:
            -- 1. Output that it's been covered
            -- 2. Call the adapted name
            --
            -- The adapted name will now:
            -- 1. Definitely be unique
            -- 2. Have the original body of the function.
            FunBind x originalName adaptedMatches [],
            FunBind
              NoExtField
              adaptedName
              originalMatches
              originalTicks
          ]
  b -> pure [b]

applyAdapterExpr :: GHC.Module -> Location -> HsExpr GhcPs -> HsExpr GhcPs
applyAdapterExpr moduule loc e =
  let strToLog =
        unwords
          [ unitToString (moduleUnit moduule),
            moduleNameString (moduleName moduule),
            locationString loc
          ]
   in HsApp
        NoExtField
        ( noLoc
            ( HsApp
                NoExtField
                (noLoc (HsVar NoExtField (noLoc (Qual adapterModuleName (mkVarOcc "adaptValue")))))
                (noLoc (HsLit NoExtField (HsString NoSourceText (mkFastString strToLog))))
            )
        )
        (noLoc e)

spanLocation :: SrcSpan -> Maybe Location
spanLocation span = case span of
  RealSrcSpan s _ ->
    Just
      Location
        { locationLine = fromIntegral (srcSpanStartLine s),
          locationColumnStart = fromIntegral (srcSpanStartCol s),
          locationColumnEnd = fromIntegral (srcSpanEndCol s)
        }
  UnhelpfulSpan _ -> Nothing

rdrNameToString :: GHC.Unit -> RdrName -> String
rdrNameToString unit n =
  unwords $
    concat
      [ [unitToString unit],
        case n of
          Unqual on -> ["unknown", occNameString on]
          Qual mn on -> [moduleNameString mn, occNameString on]
          _ -> ["unknown", "unknown"]
      ]

-- We drop the hash because it differs in a cabal build versus a nix build.
unitToString :: GHC.Unit -> String
unitToString = T.unpack . T.intercalate "-" . drop 1 . reverse . T.splitOn "-" . T.pack . unitString

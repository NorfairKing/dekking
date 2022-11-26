{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.SourceAdapter (adaptLocatedHsModule, unitToString) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import qualified Data.Set as S
import qualified Data.Text as T
import Dekking.Coverable
import GHC hiding (moduleName)
import GHC.Driver.Types as GHC
import GHC.Plugins as GHC

addCoverableTopLevelBinding :: Coverable TopLevelBinding -> AdaptM ()
addCoverableTopLevelBinding e = tell (mempty {moduleCoverablesTopLevelBindings = S.singleton e})

addExpression :: Coverable Expression -> AdaptM ()
addExpression e = tell (mempty {moduleCoverablesExpressions = S.singleton e})

type AdaptM = WriterT ModuleCoverables (ReaderT GHC.Module Hsc)

adapterImport :: LImportDecl GhcPs
adapterImport = noLoc (simpleImportDecl adapterModuleName)

adapterModuleName :: GHC.ModuleName
adapterModuleName = mkModuleName "Dekking.ValueLevelAdapter"

adaptLocatedHsModule :: Located HsModule -> AdaptM (Located HsModule)
adaptLocatedHsModule = liftL adaptHsModule

adaptHsModule :: HsModule -> AdaptM HsModule
adaptHsModule m = do
  moduule <- ask
  liftIO $ putStrLn $ "Adapting module: " ++ moduleNameString (moduleName moduule)
  decls' <- concat <$> mapM adaptLocatedTopLevelDecl (hsmodDecls m)
  pure (m {hsmodDecls = decls', hsmodImports = adapterImport : hsmodImports m})

adaptLocatedTopLevelDecl :: Located (HsDecl GhcPs) -> AdaptM [Located (HsDecl GhcPs)]
adaptLocatedTopLevelDecl lDecl = do
  lDecls' <- adaptTopLevelDecl (unLoc lDecl)
  case lDecls' of
    [] -> error "must not happen, otherwise we're deleting decls."
    [x] -> do
      x' <- adaptDecl x
      pure [L (getLoc lDecl) x'] -- Nothing was adapted at the top level
    [decl, modifiedDecl] -> do
      modifiedDecl' <- adaptDecl modifiedDecl
      pure [L (getLoc lDecl) decl, noLoc modifiedDecl']
    _ -> error "must not happen either, otherwise we're making too many extra decls"

adaptDecl :: HsDecl GhcPs -> AdaptM (HsDecl GhcPs)
adaptDecl = \case
  ValD x bind -> ValD x <$> adaptBind bind
  -- TODO
  d -> pure d

adaptBind :: HsBind GhcPs -> AdaptM (HsBind GhcPs)
adaptBind = \case
  FunBind x name matchGroup ticks -> FunBind x name <$> adaptMatchGroup matchGroup <*> pure ticks
  -- TODO
  b -> pure b

adaptMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> AdaptM (MatchGroup GhcPs (LHsExpr GhcPs))
adaptMatchGroup = \case
  MG x as origin -> MG x <$> liftL (mapM adaptLMatch) as <*> pure origin

adaptLMatch :: LMatch GhcPs (LHsExpr GhcPs) -> AdaptM (LMatch GhcPs (LHsExpr GhcPs))
adaptLMatch = liftL adaptMatch

adaptMatch :: Match GhcPs (LHsExpr GhcPs) -> AdaptM (Match GhcPs (LHsExpr GhcPs))
adaptMatch = \case
  Match x ctx pats body -> Match x ctx pats <$> adaptGRHSs body

adaptGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> AdaptM (GRHSs GhcPs (LHsExpr GhcPs))
adaptGRHSs = \case
  GRHSs x rhs localBinds -> GRHSs x <$> mapM adaptLGRHS rhs <*> adaptLocalBinds localBinds

adaptLGRHS ::
  LGRHS GhcPs (LHsExpr GhcPs) ->
  AdaptM (LGRHS GhcPs (LHsExpr GhcPs))
adaptLGRHS = liftL adaptGRHS

adaptGRHS :: GRHS GhcPs (LHsExpr GhcPs) -> AdaptM (GRHS GhcPs (LHsExpr GhcPs))
adaptGRHS = \case
  GRHS x guards body -> GRHS x guards <$> adaptLExpr body

adaptLocalBinds :: LHsLocalBinds GhcPs -> AdaptM (LHsLocalBinds GhcPs)
adaptLocalBinds = pure -- moduule = \case
--   -- TODO
--   HsValBinds x ->
--   lbs -> pure lbs

adaptLExpr :: LHsExpr GhcPs -> AdaptM (LHsExpr GhcPs)
adaptLExpr (L sp e) = fmap (L sp) $ do
  let applyAdapter mName = case spanLocation sp of
        Just loc -> do
          addExpression
            Coverable
              { coverableValue = Expression {expressionIdentifier = mName},
                coverableLocation = loc
              }
          applyAdapterExpr loc e
        Nothing -> pure e

  -- We cannot use uniplate's method of transforming the code, because it would
  -- replace the middle part of infix operations by an expression that contains
  -- multiple pieces, and GHC (correctly) assumes that that is not possible.
  -- So we have to use the manual traversal.
  case e of
    HsVar _ (L _ rdr) -> applyAdapter $ Just $ occNameString $ rdrNameOcc rdr
    HsUnboundVar x on -> pure $ HsUnboundVar x on
    HsConLikeOut x cl -> pure $ HsConLikeOut x cl
    HsApp x left right -> HsApp x <$> adaptLExpr left <*> adaptLExpr right
    HsPar x le -> HsPar x <$> adaptLExpr le
    HsDo x ctx stmts -> HsDo x ctx <$> liftL (mapM adaptExprLStmt) stmts
    OpApp x left middle right ->
      OpApp x
        <$> adaptLExpr left
        -- We cannot transform the middle part of an infix operator expression
        -- because then it would consist of more than one part.
        -- This would break GHC's assumption that infix operator expressions
        -- only consist of one part, and would cause transformations of an
        -- expression like
        -- print $ succ $ 5
        -- which is
        -- print $ (succ $ 5)
        -- to result in this expression:
        -- ((f ($))
        --  ((f (($))
        --   (f print)
        --   (f succ)))
        --  (f 5))
        -- instead of this expression:
        -- ((f ($))
        --  (f print)
        --  ((f ($))
        --   (f succ)
        --   (f 5)))
        -- , which fails to parse
        <*> pure middle
        <*> adaptLExpr right
    -- TODO
    _ -> pure e

adaptExprLStmt ::
  ExprLStmt GhcPs ->
  AdaptM (ExprLStmt GhcPs)
adaptExprLStmt = liftL $ \case
  LastStmt x e mb se -> LastStmt x <$> adaptLExpr e <*> pure mb <*> pure se
  BindStmt x p e -> BindStmt x p <$> adaptLExpr e
  BodyStmt x e se1 se2 -> BodyStmt x <$> adaptLExpr e <*> pure se1 <*> pure se2
  LetStmt x lbs -> LetStmt x <$> adaptLocalBinds lbs
  s -> pure s -- TODO

adaptTopLevelDecl :: HsDecl GhcPs -> AdaptM [HsDecl GhcPs]
adaptTopLevelDecl = \case
  ValD x bind -> fmap (ValD x) <$> adaptTopLevelBind bind
  SigD x sig -> (: []) . SigD x <$> duplicateTopLevelSig sig
  d -> pure [d]

duplicateTopLevelSig :: Sig GhcPs -> AdaptM (Sig GhcPs)
duplicateTopLevelSig = \case
  TypeSig x ls typ -> do
    moduule <- ask
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

adaptTopLevelBind :: HsBind GhcPs -> AdaptM [HsBind GhcPs]
adaptTopLevelBind = \case
  FunBind x originalName originalMatches originalTicks -> do
    let on = rdrNameOcc (unLoc originalName)
        nameString = occNameString on
    liftIO $ putStrLn $ "Adapting bind: " ++ nameString
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
        expr' <- applyAdapterExpr loc (HsVar NoExtField adaptedName)
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
                                                (noLoc expr')
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

applyAdapterExpr :: Location -> HsExpr GhcPs -> AdaptM (HsExpr GhcPs)
applyAdapterExpr loc e = do
  moduule <- ask
  let strToLog = mkStringToLog moduule loc
  pure $
    HsPar NoExtField $
      noLoc $
        HsApp
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
spanLocation sp = case sp of
  RealSrcSpan s _ ->
    Just
      Location
        { locationLine = fromIntegral (srcSpanStartLine s),
          locationColumnStart = fromIntegral (srcSpanStartCol s),
          locationColumnEnd = fromIntegral (srcSpanEndCol s)
        }
  UnhelpfulSpan _ -> Nothing

mkStringToLog :: GHC.Module -> Location -> String
mkStringToLog moduule loc =
  unwords
    [ unitToString (moduleUnit moduule),
      moduleNameString (moduleName moduule),
      locationString loc
    ]

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
unitToString u =
  case reverse . T.splitOn "-" . T.pack $ unitString u of
    [] -> "-"
    -- If there's only one component, it's probably the "main" package, and we
    -- still want to see this instead of drop it
    [x] -> T.unpack x
    -- If there is more than one component, the last component is the hash, so
    -- we drop it.
    (_ : rest) -> T.unpack $ T.intercalate "-" $ reverse rest

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.SourceAdapter (adaptLocatedHsModule, unitToString) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import Dekking.Coverable
import GHC hiding (moduleName)
import GHC.Data.Bag
import GHC.Plugins as GHC
import GHC.Types.SourceText as GHC

addExpression :: Coverable Expression -> AdaptM ()
addExpression e = tell (mempty {moduleCoverablesExpressions = S.singleton e})

type AdaptM = WriterT ModuleCoverables (ReaderT GHC.Module Hsc)

adapterImport :: LImportDecl GhcPs
adapterImport = noLocA (simpleImportDecl adapterModuleName)

adapterModuleName :: GHC.ModuleName
adapterModuleName = mkModuleName "Dekking.ValueLevelAdapter"

adaptLocatedHsModule :: Located (HsModule GhcPs) -> AdaptM (Located (HsModule GhcPs))
adaptLocatedHsModule = traverse adaptHsModule

adaptHsModule :: HsModule GhcPs -> AdaptM (HsModule GhcPs)
adaptHsModule m = do
  let annotations = gatherAnnotations m
  if DontCoverModule `elem` annotations
    then pure m
    else do
      moduule <- ask
      liftIO $ putStrLn $ "Adapting module: " ++ moduleNameString (moduleName moduule)
      decls' <- mapM (adaptTopLevelLDecl annotations) (hsmodDecls m)
      pure
        ( m
            { hsmodDecls = decls',
              hsmodImports =
                -- See [ref:ThePlanTM]
                adapterImport : hsmodImports m
            }
        )

-- | Annotations that guide the plugin
-- [tag:DisablingCoverage]
data CoverAnnotation
  = DontCoverModule
  | DontCoverFunction String
  deriving (Show, Eq)

gatherAnnotations :: HsModule GhcPs -> [CoverAnnotation]
gatherAnnotations = mapMaybe (gatherAnnotationDecl . unLoc) . hsmodDecls

gatherAnnotationDecl :: HsDecl GhcPs -> Maybe CoverAnnotation
gatherAnnotationDecl = \case
  AnnD _ (HsAnnotation _ p body) -> do
    guard $ isNoCoverExpr body
    case p of
      ValueAnnProvenance rdr -> Just $ DontCoverFunction $ occNameString $ rdrNameOcc $ unLoc rdr
      ModuleAnnProvenance -> Just DontCoverModule
      _ -> Nothing
  _ -> Nothing

isNoCoverExpr :: LHsExpr GhcPs -> Bool
isNoCoverExpr expr = case unLoc expr of
  HsLit _ (HsString _ fs) | "NOCOVER" `isInfixOf` unpackFS fs -> True
  HsOverLit _ (OverLit _ (HsIsString _ fs)) | "NOCOVER" `isInfixOf` unpackFS fs -> True
  HsPar _ _ e _ -> isNoCoverExpr e
  ExprWithTySig _ e _ -> isNoCoverExpr e
  _ -> False

adaptTopLevelLDecl :: [CoverAnnotation] -> LHsDecl GhcPs -> AdaptM (LHsDecl GhcPs)
adaptTopLevelLDecl annotations = traverse $ adaptTopLevelDecl annotations

adaptTopLevelDecl :: [CoverAnnotation] -> HsDecl GhcPs -> AdaptM (HsDecl GhcPs)
adaptTopLevelDecl annotations = \case
  TyClD x tydcl -> TyClD x <$> adaptTypeOrClassDecl tydcl
  InstD x instdcl -> InstD x <$> adaptInstanceDecl instdcl
  ValD x bind -> ValD x <$> adaptTopLevelBind annotations bind
  -- TODO
  d -> pure d

adaptTypeOrClassDecl ::
  TyClDecl GhcPs ->
  AdaptM (TyClDecl GhcPs)
adaptTypeOrClassDecl = \case
  cd@ClassDecl {} -> do
    lbs <- adaptLBinds (tcdMeths cd)
    pure (cd {tcdMeths = lbs})
  d -> pure d

adaptInstanceDecl ::
  InstDecl GhcPs -> AdaptM (InstDecl GhcPs)
adaptInstanceDecl = \case
  ClsInstD x cinstdcl -> ClsInstD x <$> adaptClassInstanceDecl cinstdcl
  d -> pure d

adaptClassInstanceDecl ::
  ClsInstDecl GhcPs -> AdaptM (ClsInstDecl GhcPs)
adaptClassInstanceDecl = \case
  cid@ClsInstDecl {} -> do
    lbs <- adaptLBinds (cid_binds cid)
    pure (cid {cid_binds = lbs})

adaptTopLevelBind :: [CoverAnnotation] -> HsBind GhcPs -> AdaptM (HsBind GhcPs)
adaptTopLevelBind annotations = \case
  b@(FunBind _ name _) ->
    if DontCoverFunction (occNameString (rdrNameOcc (unLoc name))) `elem` annotations
      then pure b
      else adaptBind b
  -- TODO
  b -> pure b

adaptLBinds :: LHsBinds GhcPs -> AdaptM (LHsBinds GhcPs)
adaptLBinds = mapBagM adaptLBind

adaptLBind :: LHsBind GhcPs -> AdaptM (LHsBind GhcPs)
adaptLBind = traverse adaptBind

adaptBind :: HsBind GhcPs -> AdaptM (HsBind GhcPs)
adaptBind = \case
  FunBind x name matchGroup -> FunBind x name <$> adaptMatchGroup matchGroup
  -- TODO
  b -> pure b

adaptMatchGroup :: MatchGroup GhcPs (LHsExpr GhcPs) -> AdaptM (MatchGroup GhcPs (LHsExpr GhcPs))
adaptMatchGroup = \case
  MG x as -> MG x <$> traverse (mapM adaptLMatch) as

adaptLMatch :: LMatch GhcPs (LHsExpr GhcPs) -> AdaptM (LMatch GhcPs (LHsExpr GhcPs))
adaptLMatch = traverse adaptMatch

adaptMatch :: Match GhcPs (LHsExpr GhcPs) -> AdaptM (Match GhcPs (LHsExpr GhcPs))
adaptMatch = \case
  Match x ctx pats body -> Match x ctx pats <$> adaptGRHSs body

adaptGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> AdaptM (GRHSs GhcPs (LHsExpr GhcPs))
adaptGRHSs = \case
  GRHSs x rhs localBinds -> GRHSs x <$> mapM adaptLGRHS rhs <*> adaptHsLocalBinds localBinds

adaptLGRHS ::
  LGRHS GhcPs (LHsExpr GhcPs) ->
  AdaptM (LGRHS GhcPs (LHsExpr GhcPs))
adaptLGRHS = traverse adaptGRHS

adaptGRHS :: GRHS GhcPs (LHsExpr GhcPs) -> AdaptM (GRHS GhcPs (LHsExpr GhcPs))
adaptGRHS = \case
  GRHS x guards body -> GRHS x guards <$> adaptLExpr body

adaptHsLocalBinds :: HsLocalBinds GhcPs -> AdaptM (HsLocalBinds GhcPs)
adaptHsLocalBinds = \case
  HsValBinds x valBinds -> HsValBinds x <$> adaptValBinds valBinds
  lbs -> pure lbs

adaptValBinds :: HsValBinds GhcPs -> AdaptM (HsValBinds GhcPs)
adaptValBinds = \case
  ValBinds x binds sigs -> ValBinds x <$> mapBagM adaptLBind binds <*> pure sigs
  XValBindsLR (NValBinds binds sigs) ->
    XValBindsLR
      <$> ( NValBinds
              <$> mapM
                ( \(f, s) ->
                    (,) f <$> mapBagM adaptLBind s
                )
                binds
              <*> pure sigs
          )

--   -- TODO
--   HsValBinds x ->
--   lbs -> pure lbs

-- [tag:NoUniplate]
-- We cannot use uniplate's method of transforming the code, because it would
-- replace the middle part of infix operations by an expression that contains
-- multiple pieces, and GHC (correctly) assumes that that is not possible.
-- So we have to use the manual traversal.
--
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

adaptLExpr :: LHsExpr GhcPs -> AdaptM (LHsExpr GhcPs)
adaptLExpr le = traverse (adaptExpr (getLocA le)) le

adaptExpr :: SrcSpan -> HsExpr GhcPs -> AdaptM (HsExpr GhcPs)
adaptExpr sp e = do
  let applyAdapter mName = case spanLocation sp of
        Just loc -> do
          addExpression
            Coverable
              { coverableValue = Dekking.Coverable.Expression {expressionIdentifier = mName},
                coverableLocation = loc
              }
          applyAdapterExpr loc e
        Nothing -> pure e

  -- [ref:NoUniplate]
  case e of
    HsVar _ (L _ rdr) -> applyAdapter $ Just $ occNameString $ rdrNameOcc rdr
    HsUnboundVar x on -> pure $ HsUnboundVar x on
    HsOverLabel x s fs -> pure $ HsOverLabel x s fs
    HsIPVar x iv -> pure $ HsIPVar x iv
    HsOverLit {} -> applyAdapter Nothing
    HsLit {} -> applyAdapter Nothing
    HsLam x mg -> HsLam x <$> adaptMatchGroup mg
    HsLamCase x v mg -> HsLamCase x v <$> adaptMatchGroup mg
    HsApp x left right -> HsApp x <$> adaptLExpr left <*> adaptLExpr right
    -- TODO: Things inside a visible type application might be covered more
    -- granularly but this is quite good in the meantime.
    HsAppType {} -> applyAdapter Nothing
    OpApp x left middle right ->
      OpApp x
        <$> adaptLExpr left
        -- [ref:NoUniplate]
        <*> pure middle
        <*> adaptLExpr right
    NegApp x body se -> NegApp x <$> adaptLExpr body <*> pure se
    HsPar x l le r -> HsPar x l <$> adaptLExpr le <*> pure r
    ExplicitTuple x args boxity -> ExplicitTuple x <$> mapM adaptTupArg args <*> pure boxity
    ExplicitSum x ct a body -> ExplicitSum x ct a <$> adaptLExpr body
    HsCase x body mg -> HsCase x <$> adaptLExpr body <*> adaptMatchGroup mg
    HsIf x condE ifE elseE -> HsIf x <$> adaptLExpr condE <*> adaptLExpr ifE <*> adaptLExpr elseE
    HsLet x l lbs i body -> HsLet x l <$> adaptHsLocalBinds lbs <*> pure i <*> adaptLExpr body
    HsDo x ctx stmts -> HsDo x ctx <$> traverse (mapM adaptExprLStmt) stmts
    ExplicitList x bodies -> ExplicitList x <$> mapM adaptLExpr bodies
    RecordCon x name binds -> RecordCon x name <$> adaptRecordBinds binds
    RecordUpd x left updates ->
      RecordUpd x
        <$> adaptLExpr left
        <*> either
          (fmap Left . mapM adaptLRecordUpdateField)
          (fmap Right . mapM adaptLRecordUpdateProjection)
          updates
    -- TODO
    _ -> pure e

adaptTupArg :: HsTupArg GhcPs -> AdaptM (HsTupArg GhcPs)
adaptTupArg = \case
  Present x body -> Present x <$> adaptLExpr body
  Missing x -> pure $ Missing x

adaptRecordBinds :: HsRecordBinds GhcPs -> AdaptM (HsRecordBinds GhcPs)
adaptRecordBinds = \case
  HsRecFields fields md -> HsRecFields <$> mapM (traverse adaptHsRecField') fields <*> pure md

adaptLRecordUpdateProjection :: LHsRecUpdProj GhcPs -> AdaptM (LHsRecUpdProj GhcPs)
adaptLRecordUpdateProjection = traverse $ \case
  HsFieldBind ex i e b -> HsFieldBind ex i <$> adaptLExpr e <*> pure b

adaptLRecordUpdateField :: LHsRecUpdField GhcPs -> AdaptM (LHsRecUpdField GhcPs)
adaptLRecordUpdateField = traverse adaptRecordUpdateField

adaptRecordUpdateField :: HsRecUpdField GhcPs -> AdaptM (HsRecUpdField GhcPs)
adaptRecordUpdateField = \case
  HsFieldBind ex i e b -> HsFieldBind ex i <$> adaptLExpr e <*> pure b

adaptHsRecField' :: HsRecField id (LHsExpr GhcPs) -> AdaptM (HsRecField id (LHsExpr GhcPs))
adaptHsRecField' = \case
  HsFieldBind ex i e b -> HsFieldBind ex i <$> adaptLExpr e <*> pure b

adaptExprLStmt ::
  ExprLStmt GhcPs ->
  AdaptM (ExprLStmt GhcPs)
adaptExprLStmt = traverse $ \case
  LastStmt x e mb se -> LastStmt x <$> adaptLExpr e <*> pure mb <*> pure se
  BindStmt x p e -> BindStmt x p <$> adaptLExpr e
  BodyStmt x e se1 se2 -> BodyStmt x <$> adaptLExpr e <*> pure se1 <*> pure se2
  LetStmt x lbs -> LetStmt x <$> adaptHsLocalBinds lbs
  s -> pure s -- TODO

-- See [ref:ThePlanTM]
applyAdapterExpr :: Location -> HsExpr GhcPs -> AdaptM (HsExpr GhcPs)
applyAdapterExpr loc e = do
  moduule <- ask
  let strToLog = mkStringToLog moduule loc
  pure $
    HsPar
      EpAnnNotUsed
      noHsTok
      ( noLocA $
          HsApp
            EpAnnNotUsed
            ( noLocA
                ( HsApp
                    EpAnnNotUsed
                    (noLocA (HsVar NoExtField (noLocA (Qual adapterModuleName (mkVarOcc "adaptValue")))))
                    (noLocA (HsLit EpAnnNotUsed (HsString NoSourceText (mkFastString strToLog))))
                )
            )
            (noLocA e)
      )
      noHsTok

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

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Dekking.SourceAdapter (adaptLocatedHsModule) where

import Control.Monad.Reader
import Control.Monad.Writer.Strict
import Data.Set (Set)
import qualified Data.Set as S
import Dekking.Coverable
import GHC hiding (moduleName)
import GHC.Driver.Types as GHC
import GHC.Plugins as GHC hiding (moduleName)

addCoverableTopLevelBinding :: Coverable TopLevelBinding -> AdaptM ()
addCoverableTopLevelBinding a = tell (S.singleton a)

type AdaptM = WriterT (Set (Coverable TopLevelBinding)) Hsc

adapterImport :: LImportDecl GhcPs
adapterImport = noLoc (simpleImportDecl adapterModuleName)

adapterModuleName :: GHC.ModuleName
adapterModuleName = mkModuleName "Dekking.ValueLevelAdapter"

adaptLocatedHsModule :: GHC.ModuleName -> Located HsModule -> AdaptM (Located HsModule)
adaptLocatedHsModule moduleName = adaptLocated (adaptHsModule moduleName)

adaptHsModule :: GHC.ModuleName -> HsModule -> AdaptM HsModule
adaptHsModule moduleName m = do
  liftIO $ putStrLn $ "Adapting module: " ++ moduleNameString moduleName
  decls' <- concat <$> mapM (adaptLocatedTopLevelDecl moduleName) (hsmodDecls m)
  pure (m {hsmodDecls = decls', hsmodImports = adapterImport : hsmodImports m})

adaptLocatedTopLevelDecl :: GHC.ModuleName -> Located (HsDecl GhcPs) -> AdaptM [Located (HsDecl GhcPs)]
adaptLocatedTopLevelDecl moduleName lDecl = do
  lDecls' <- adaptTopLevelDecl moduleName (unLoc lDecl)
  case lDecls' of
    [] -> error "must not happen, otherwise we're deleting decls."
    [x] -> pure [L (getLoc lDecl) x] -- Nothing was adapted
    [decl, modifiedDecl] -> do
      pure [L (getLoc lDecl) decl, noLoc modifiedDecl]
    _ -> error "must not happen either, otherwise we're making too many extra decls"

adaptTopLevelDecl :: GHC.ModuleName -> HsDecl GhcPs -> AdaptM [HsDecl GhcPs]
adaptTopLevelDecl moduleName = \case
  ValD x bind -> fmap (ValD x) <$> adaptBind moduleName bind
  SigD x sig -> (: []) . SigD x <$> adaptSig sig
  d -> pure [d]

adaptSig :: Sig GhcPs -> AdaptM (Sig GhcPs)
adaptSig = \case
  TypeSig x ls typ -> do
    let nameStrings = map (rdrNameToString . unLoc) ls
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

adaptBind :: GHC.ModuleName -> HsBind GhcPs -> AdaptM [HsBind GhcPs]
adaptBind moduleName = \case
  FunBind x originalName originalMatches originalTicks -> do
    let nameString = rdrNameToString (unLoc originalName)
    liftIO $ putStrLn $ "Adapting bind: " ++ nameString
    adaptedName <- noLoc <$> adaptTopLevelName (unLoc originalName)
    let strToLog = rdrNameToString (mkRdrQual moduleName (rdrNameOcc (unLoc originalName)))
    let
    addCoverableTopLevelBinding
      Coverable
        { coverableValue = TopLevelBinding {topLevelBindingIdentifier = nameString},
          coverableLocation = case getLoc originalName of
            RealSrcSpan s _ ->
              Just
                Location
                  { locationLine = fromIntegral (srcSpanStartLine s),
                    locationColumnStart = fromIntegral (srcSpanStartCol s),
                    locationColumnEnd = fromIntegral (srcSpanEndCol s)
                  }
            UnhelpfulSpan _ -> Nothing
        }
    let applyAdapter :: HsExpr GhcPs -> HsExpr GhcPs
        applyAdapter e =
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
                                            (noLoc (applyAdapter (HsVar NoExtField adaptedName)))
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

rdrNameToString :: RdrName -> String
rdrNameToString = \case
  Unqual on -> occNameString on
  Qual mn on -> moduleNameString mn ++ "." ++ occNameString on
  _ -> "unknown"

adaptLocated :: Monad m => (a -> m b) -> Located a -> m (Located b)
adaptLocated = liftL

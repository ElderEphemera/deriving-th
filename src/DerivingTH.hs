{-# language CPP #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}

module DerivingTH (plugin) where

import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Generics

import GHC.Hs
#if __GLASGOW_HASKELL__ < 900
import GhcPlugins
import OccName as NS
#else
import GHC.Plugins
import GHC.Types.Name.Occurrence as NS
#endif
#if __GLASGOW_HASKELL__ >= 902
import GHC.Types.SourceText
#endif

import Language.Haskell.TH.LanguageExtensions


--------------------------------------------------------------------------------
-- Main definitions
--------------------------------------------------------------------------------

plugin :: Plugin
plugin = defaultPlugin
  { pluginRecompile = purePlugin
#if __GLASGOW_HASKELL__ < 902
  , dynflagsPlugin = \_opts -> pure . addExtensions
#else
  , driverPlugin = \_opts env -> pure $
      env { hsc_dflags = addExtensions $ hsc_dflags env }
#endif
  , parsedResultAction = \_opts _summary -> pure . processModule
  }

addExtensions :: DynFlags -> DynFlags
addExtensions dflags = foldl' xopt_set dflags
  [ DerivingVia
  , TemplateHaskell
  , TemplateHaskellQuotes
  ]

processModule :: HsParsedModule -> HsParsedModule
processModule hpm@HsParsedModule{ hpm_module = L l hsm } = hpm
  { hpm_module = L l $ hsm
    { hsmodImports = internalImport : hsmodImports hsm
    , hsmodDecls = everywhere (mkT processDecls) $ hsmodDecls hsm
    }
  }

internalImport :: LImportDecl GhcPs
internalImport = gen $ ImportDecl
  { ideclExt = noAnn
  , ideclSourceSrc = NoSourceText
  , ideclName = gen $ mkModuleName "DerivingTH.Internal"
  , ideclPkgQual = Nothing
#if __GLASGOW_HASKELL__ < 900
  , ideclSource = False
#else
  , ideclSource = NotBoot
#endif
  , ideclSafe = False
  , ideclQualified = QualifiedPre
  , ideclImplicit = True
  , ideclAs = Nothing
  , ideclHiding = Nothing
  }

--------------------------------------------------------------------------------
-- Process Declarations
--------------------------------------------------------------------------------

processDecls :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
processDecls = concatMap processDecl

processDecl :: LHsDecl GhcPs -> [LHsDecl GhcPs]
processDecl (L loc (DerivD _ derivDecl)) = [L loc $ processDerivDecl derivDecl]
processDecl decl@(L _ (TyClD _ (DataDecl _ tyname _ _ _))) =
  let (derivDecls, decl') = everywhereM (mkM $ processDerivClauses tyname) decl
  in decl' : derivDecls
processDecl decl = [decl]

processDerivDecl :: DerivDecl GhcPs -> HsDecl GhcPs
processDerivDecl (DerivDecl _ sigty strat _overlap)
  | isTemplate strat
  , IgnoredBinders ty <- sigty
  , L _ (HsAppTy _ cls subjTy) <- ty
  = makeSplice (TypeSubject subjTy) cls
processDerivDecl derivDecl = DerivD nxf derivDecl

processDerivClauses
  :: LIdP GhcPs
  -> [LHsDerivingClause GhcPs]
  -> ([LHsDecl GhcPs], [LHsDerivingClause GhcPs])
processDerivClauses tyname =
  first concat . partitionEithers . map (processDerivClause tyname)

processDerivClause
  :: LIdP GhcPs
  -> LHsDerivingClause GhcPs
  -> Either [LHsDecl GhcPs] (LHsDerivingClause GhcPs)
processDerivClause tyname (L l (HsDerivingClause _ strat (L _ clss)))
  | isTemplate strat
  = Left $ L (locAnn l) . makeSplice (NameSubject tyname) . hsib_body
    <$> derivClauseTys clss
processDerivClause _tyname derivClause = Right derivClause

isTemplate :: Maybe (LDerivStrategy GhcPs) -> Bool
isTemplate (Just (L _ (ViaStrategy via)))
#if __GLASGOW_HASKELL__ < 902
  | HsIB _ ty <- via
#else
  | XViaStrategyPs _ (L _ (HsSig _ _ ty)) <- via
#endif
  , L _ (HsTyVar _ NotPromoted (L _ (Unqual occ))) <- ty
  = occNameFS occ == "template"
isTemplate _ = False

makeSplice :: Subject -> LHsType GhcPs -> HsDecl GhcPs
makeSplice subj cls = SpliceD nxf $ SpliceDecl nxf
  (gen $ HsUntypedSplice noAnn dollarSplice (mkUnqual NS.varName "splice") $
     foldl' mkHsApp method [proxy, subjectExpr subj])
  ExplicitSplice
  where
    method = internalVar NS.varName $ subjectMethod subj
    proxy = gen . ExprWithTySig noAnn (internalVar dataName "Proxy") .
      IgnoredBinders $ mkHsAppTy (internalTyVar tcName "Proxy") cls

data Subject
  = NameSubject (LIdP GhcPs)
  | TypeSubject (LHsType GhcPs)

subjectMethod :: Subject -> FastString
subjectMethod (NameSubject _) = "deriveTH'"
subjectMethod (TypeSubject _) = "deriveTHType'"

subjectExpr :: Subject -> LHsExpr GhcPs
#if __GLASGOW_HASKELL__ < 902
subjectExpr (NameSubject name) = HsBracket noAnn . VarBr nxf True <$> name
#else
subjectExpr (NameSubject name) = gen . HsBracket noAnn . VarBr nxf True $ name
#endif
subjectExpr (TypeSubject ty) = gen . HsBracket noAnn . ExpBr nxf . gen $
  ExprWithTySig noAnn (internalVar NS.varName "placeholder") (IgnoredBinders ty)


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

nxf :: NoExtField
nxf = noExtField

#if __GLASGOW_HASKELL__ < 902
noAnn :: NoExtField
noAnn = noExtField
#endif

#if __GLASGOW_HASKELL__ < 902
locAnn :: SrcSpan -> SrcSpan
locAnn = id
#else
locAnn :: SrcSpan -> SrcSpanAnn' (EpAnn ann)
locAnn = SrcSpanAnn noAnn
#endif

#if __GLASGOW_HASKELL__ < 902
gen :: a -> Located a
#else
gen :: a -> GenLocated (SrcSpanAnn' (EpAnn ann)) a
#endif
gen = L . locAnn $ mkGeneralSrcSpan "<generated by deriving-th>"

internal :: NameSpace -> FastString -> LIdP GhcPs
internal ns name = gen $ mkQual ns ("DerivingTH.Internal", name)

internalVar :: NameSpace -> FastString -> LHsExpr GhcPs
internalVar ns = gen . HsVar nxf . internal ns

internalTyVar :: NameSpace -> FastString -> LHsType GhcPs
internalTyVar ns = gen . HsTyVar noAnn NotPromoted . internal ns

#if __GLASGOW_HASKELL__ < 902
derivClauseTys :: [LHsSigType GhcPs] -> [LHsSigType GhcPs]
derivClauseTys = id
#else
derivClauseTys :: DerivClauseTys GhcPs -> [LHsType GhcPs]
derivClauseTys dct
  | DctSingle _ sigty <- dct = [unsig $ sigty]
  | DctMulti _ sigtys <- dct = unsig <$> sigtys
  where unsig (L _ (HsSig _ _ ty)) = ty :: LHsType GhcPs
#endif

#if __GLASGOW_HASKELL__ >= 902
hsib_body :: LHsType GhcPs -> LHsType GhcPs
hsib_body = id
#endif

dollarSplice :: SpliceDecoration
#if __GLASGOW_HASKELL__ < 900
dollarSplice = HasDollar
#else
dollarSplice = DollarSplice
#endif

pattern IgnoredBinders :: LHsType GhcPs -> LHsSigWcType GhcPs
#if __GLASGOW_HASKELL__ < 902
pattern IgnoredBinders ty <- HsWC _ (HsIB _ ty)
  where IgnoredBinders ty = HsWC nxf (HsIB nxf ty)
#else
pattern IgnoredBinders ty <- HsWC _ (L _ (HsSig _ _ ty))
  where IgnoredBinders ty = HsWC nxf (gen (HsSig nxf (HsOuterImplicit nxf) ty))
#endif

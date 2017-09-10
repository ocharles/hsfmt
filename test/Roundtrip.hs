{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}
{-# language TemplateHaskell #-}
{-# language TypeSynonymInstances #-}

module Main where

import BasicTypes (Boxity(Boxed), Origin(FromSource))
import TcEvidence (HsWrapper(WpHole))
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import qualified Bag
import qualified BasicTypes
import Control.Monad.IO.Class
import Data.Text.Prettyprint.Doc (pretty)
import qualified FastString
import qualified GHC
import HSFmt ()
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Language.Haskell.GHC.ExactPrint (exactPrint)
import Language.Haskell.GHC.ExactPrint.Parsers (parseModuleFromString)
import qualified RdrName
import System.IO



main :: IO ()
main =
  do
    hSetEncoding stdout utf8

    hSetEncoding stderr utf8

    checkParallel $$(discover)

    return ()


prop_moduleRoundtrip :: Property
prop_moduleRoundtrip =
  withTests 1000 $ property $ (do
                                 ShowModule mod <-
                                   forAll (fmap ShowModule genModule)

                                 parse <-
                                   liftIO (parseModuleFromString "input.hs" (show (pretty mod)))

                                 case parse of
                                   Left e ->
                                     fail (show e)

                                   Right (_, parsed) ->
                                     do
                                       footnote (ppDiff $ getGroupedDiff (lines $ show (pretty mod)) (lines $ show (pretty parsed))))


newtype ShowModule = ShowModule (GHC.HsModule GHC.RdrName)


instance Show ShowModule where
  show (ShowModule mod) =
    show (pretty mod)


class SynEq a where
  synEq =
    (==)

  synEq :: a -> a -> Bool

  default synEq :: Eq a => a -> a -> Bool


instance SynEq a => SynEq (GHC.HsModule a) where
  synEq a b =
    GHC.hsmodName a `synEq` GHC.hsmodName b && GHC.hsmodImports a `synEq` GHC.hsmodImports b && GHC.hsmodDecls a `synEq` GHC.hsmodDecls b


instance SynEq a => SynEq (GHC.HsDecl a) where
  synEq (GHC.TyClD a) (GHC.TyClD b) =
    a `synEq` b
  synEq (GHC.InstD a) (GHC.InstD b) =
    True
  synEq (GHC.ValD a) (GHC.ValD b) =
    True
  synEq (GHC.SigD a) (GHC.SigD b) =
    True


instance SynEq a => SynEq (GHC.TyClDecl a) where
  synEq (a@GHC.ClassDecl {}) (b@GHC.ClassDecl {}) =
    True
  synEq (a@GHC.DataDecl {}) (b@GHC.DataDecl {}) =
    True
  synEq (a@GHC.SynDecl {}) (b@GHC.SynDecl {}) =
    True


instance SynEq a => SynEq [a] where
  synEq (a : as) (b : bs) =
    a `synEq` b && as `synEq` bs
  synEq []  []  =
    True
  synEq _ _ =
    False


instance SynEq a => SynEq (GHC.ImportDecl a) where
  synEq a b =
    GHC.ideclName a `synEq` GHC.ideclName b && GHC.ideclPkgQual a `synEq` GHC.ideclPkgQual b && GHC.ideclSource a `synEq` GHC.ideclSource b && GHC.ideclQualified a `synEq` GHC.ideclQualified b && GHC.ideclHiding a `synEq` GHC.ideclHiding b && GHC.ideclAs a `synEq` GHC.ideclAs b


instance SynEq Bool where



instance (SynEq a, SynEq b) => SynEq (a, b) where
  synEq (a, a') (b, b') =
    a `synEq` b && a' `synEq` b'


instance (SynEq a) => SynEq (GHC.IE a) where
  synEq a b =
    GHC.ieNames a `synEq` GHC.ieNames b


instance SynEq BasicTypes.StringLiteral where



instance SynEq a => SynEq (Maybe a) where
  synEq Nothing  Nothing  =
    True
  synEq (Just a) (Just b) =
    a `synEq` b
  synEq _ _ =
    False


instance SynEq a => SynEq (GHC.Located a) where
  synEq (GHC.L _ a) (GHC.L _ b) =
    synEq a b


instance SynEq GHC.ModuleName where



instance SynEq Char where



genModule :: Gen (GHC.HsModule RdrName.RdrName)
genModule =
  do
    hsmodName <-
      Gen.maybe . located $ genModuleName

    hsmodImports <-
      Gen.list (Range.linear 0 10) (located genImportDecl)

    hsmodExports <-
      Gen.maybe . located $ Gen.list (Range.linear 0 10) (located genIE)

    hsmodDecls <-
      Gen.list (Range.linear 0 10) (located genDecl)

    hsmodDeprecMessage <-
      pure Nothing

    hsmodHaddockModHeader <-
      pure Nothing

    pure GHC.HsModule {..}


genDecl :: Gen (GHC.HsDecl RdrName.RdrName)
genDecl =
  Gen.choice [ GHC.TyClD <$> genTyClDecl
             , GHC.InstD <$> genInstDecl
             , GHC.SigD <$> genSig
             , GHC.ValD <$> genBind
             ]


genSpliceDecl :: Gen (GHC.SpliceDecl RdrName.RdrName)
genSpliceDecl =
  GHC.SpliceDecl <$> located genHsSplice <*> Gen.element [ GHC.ExplicitSplice
                                                         , GHC.ImplicitSplice
                                                         ]


genHsSplice :: Gen (GHC.HsSplice RdrName.RdrName)
genHsSplice =
  Gen.choice [GHC.HsUntypedSplice <$> genTyVar <*> located genExpr]


genInstDecl :: Gen (GHC.InstDecl RdrName.RdrName)
genInstDecl =
  Gen.choice [GHC.ClsInstD <$> genClsInstDecl]


genClsInstDecl :: Gen (GHC.ClsInstDecl RdrName.RdrName)
genClsInstDecl =
  GHC.ClsInstDecl <$> genLHsSigType <*> genBinds <*> pure [] <*> pure [] <*> pure [] <*> pure Nothing


genLDataFamInstDecl :: Gen (GHC.Located (GHC.DataFamInstDecl RdrName.RdrName))
genLDataFamInstDecl =
  located genDataFamInstDecl


genDataFamInstDecl :: Gen (GHC.DataFamInstDecl RdrName.RdrName)
genDataFamInstDecl =
  GHC.DataFamInstDecl <$> located genTypeName <*> genHsTyPats <*> genHsDataDefn <*> pure GHC.PlaceHolder


genHsTyPats :: Gen (GHC.HsImplicitBndrs RdrName.RdrName [GHC.Located (GHC.HsType RdrName.RdrName)])
genHsTyPats =
  GHC.HsIB <$> pure GHC.PlaceHolder <*> (Gen.list (Range.linear 0 10) (located genHsType))


genHsImplicitBndrs :: Gen (GHC.HsImplicitBndrs RdrName.RdrName (GHC.Located (GHC.HsType RdrName.RdrName)))
genHsImplicitBndrs =
  GHC.HsIB <$> pure GHC.PlaceHolder <*> located genHsType


genSig :: Gen (GHC.Sig RdrName.RdrName)
genSig =
  Gen.choice [ GHC.TypeSig <$> Gen.list (Range.linear 1 10) (located genVarName) <*> (GHC.HsIB <$> pure GHC.PlaceHolder <*> (GHC.HsWC <$> pure GHC.PlaceHolder <*> pure Nothing <*> located genHsType))
  ]


genTyClDecl :: Gen (GHC.TyClDecl RdrName.RdrName)
genTyClDecl =
  Gen.choice [dataDecl, synDecl, classDecl]

  where

    dataDecl =
      GHC.DataDecl <$> located genTypeName <*> genLHsQTyVars <*> genHsDataDefn <*> pure GHC.PlaceHolder <*> pure GHC.PlaceHolder

    synDecl =
      GHC.SynDecl <$> located genTypeName <*> genLHsQTyVars <*> located genHsType <*> pure GHC.PlaceHolder

    classDecl =
      GHC.ClassDecl <$> located (pure []) <*> located genTypeName <*> genLHsQTyVars <*> pure [] <*> Gen.list (Range.linear 0 10) (located $ Gen.choice [ genSig
                                                                                                                                                       , GHC.ClassOpSig <$> Gen.bool <*> Gen.list (Range.singleton 1) (located genVarName) <*> genLHsSigType
                                                                                                                                                       ]) <*> genBinds <*> pure [] <*> pure [] <*> pure [] <*> pure GHC.PlaceHolder


genLHsQTyVars :: Gen (GHC.LHsQTyVars RdrName.RdrName)
genLHsQTyVars =
  GHC.HsQTvs <$> pure GHC.PlaceHolder <*> Gen.list (Range.linear 0 10) (located genHsTyVarBndr) <*> pure GHC.PlaceHolder


genHsTyVarBndr :: Gen (GHC.HsTyVarBndr RdrName.RdrName)
genHsTyVarBndr =
  Gen.choice [GHC.UserTyVar <$> located genTyVar]


genHsDataDefn :: Gen (GHC.HsDataDefn RdrName.RdrName)
genHsDataDefn =
  do
    newOrData <-
      genNewOrData

    GHC.HsDataDefn <$> pure newOrData <*> located (Gen.list (Range.linear 0 10) (located genHsType)) <*> pure Nothing <*> Gen.maybe (located genHsKind) <*> Gen.list (if newOrData == GHC.NewType then
                                                                                                                                                                        Range.singleton 1
                                                                                                                                                                      else
                                                                                                                                                                        Range.linear 0 10) (located genConDecl) <*> genHsDeriving


genHsDeriving :: Gen (Maybe (GHC.Located [GHC.HsImplicitBndrs RdrName.RdrName (GHC.Located (GHC.HsType RdrName.RdrName))]))
genHsDeriving =
  Gen.maybe (located (Gen.list (Range.linear 0 10) genLHsSigType))


genLHsSigType :: Gen (GHC.HsImplicitBndrs RdrName.RdrName (GHC.Located (GHC.HsType RdrName.RdrName)))
genLHsSigType =
  genHsImplicitBndrs


genConDecl :: Gen (GHC.ConDecl RdrName.RdrName)
genConDecl =
  Gen.choice [ GHC.ConDeclH98 <$> located genTypeName <*> Gen.maybe genLHsQTyVars <*> Gen.maybe (located (Gen.list (Range.linear 0 10) (located genHsType))) <*> genHsConDeclDetails <*> pure Nothing
  ]


genHsConDeclDetails :: Gen (GHC.HsConDetails (GHC.Located (GHC.HsType RdrName.RdrName)) (GHC.Located [GHC.Located (GHC.ConDeclField RdrName.RdrName)]))
genHsConDeclDetails =
  Gen.choice [ GHC.PrefixCon <$> Gen.list (Range.linear 0 10) (located genHsType)
             , GHC.RecCon <$> located (Gen.list (Range.linear 0 10) (located genConDeclField))
             ]


genConDeclField :: Gen (GHC.ConDeclField RdrName.RdrName)
genConDeclField =
  GHC.ConDeclField <$> Gen.list (Range.linear 1 10) (located (GHC.FieldOcc <$> located genVarName <*> pure GHC.PlaceHolder)) <*> located genHsType <*> pure Nothing


genHsKind :: Gen (GHC.HsType RdrName.RdrName)
genHsKind =
  genHsType


genHsType :: Gen (GHC.HsType RdrName.RdrName)
genHsType =
  Gen.recursive Gen.choice [ GHC.HsTyVar <$> located genTyVar
  ] [ GHC.HsAppsTy <$> Gen.list (Range.linear 1 3) (located (GHC.HsAppPrefix <$> located genHsType))
    , Gen.subterm2 genHsType genHsType $ (\a b ->
      GHC.HsAppTy (GHC.L srcSpan a) (GHC.L srcSpan b))
    , Gen.subterm2 genHsType genHsType $ (\a b ->
      GHC.HsFunTy (GHC.L srcSpan a) (GHC.L srcSpan b))
    , Gen.subtermM genHsType $ (\expr ->
      GHC.HsListTy <$> located (pure expr))
    ]


genHsTypeCtx :: Gen (GHC.HsType RdrName.RdrName)
genHsTypeCtx =
  Gen.choice [ genHsType
             , GHC.HsQualTy <$> located (Gen.list (Range.linear 0 3) (located genHsType)) <*> located genHsType
             ]


genNewOrData :: Gen GHC.NewOrData
genNewOrData =
  Gen.choice [pure GHC.NewType, pure GHC.DataType]


genBind :: Gen (GHC.HsBindLR RdrName.RdrName RdrName.RdrName)
genBind =
  Gen.choice [ GHC.VarBind <$> genVarName <*> located genExpr <*> Gen.bool
             , GHC.FunBind <$> located genVarName <*> (GHC.MG <$> located (Gen.list (Range.singleton 1) (located $ GHC.Match <$> pure GHC.NonFunBindMatch <*> Gen.list (Range.linear 0 10) (located genPat) <*> pure Nothing <*> grhsss)) <*> pure [] <*> pure GHC.PlaceHolder <*> pure FromSource) <*> pure WpHole <*> pure GHC.PlaceHolder <*> pure []
             , GHC.PatBind <$> located genPat <*> grhsss <*> pure GHC.PlaceHolder <*> pure GHC.PlaceHolder <*> pure ([], [])
             ]

  where

    grhsss =
      GHC.GRHSs <$> Gen.list (Range.linear 1 3) (located genGRHS) <*> located (Gen.choice [ pure GHC.EmptyLocalBinds
                                                                                          , GHC.HsValBinds <$> (GHC.ValBindsIn <$> genBinds <*> pure [])
                                                                                          ])


genBinds :: Gen (GHC.LHsBinds RdrName.RdrName)
genBinds =
  Gen.recursive Gen.choice [ fmap (Bag.listToBag . map (GHC.L GHC.noSrcSpan)) $ Gen.list (Range.singleton 1) (GHC.VarBind <$> genVarName <*> located genExpr <*> Gen.bool)
  ] [ Gen.subtermM genBinds (\localBinds ->
    fmap (Bag.listToBag . map (GHC.L GHC.noSrcSpan)) $ Gen.list (Range.linear 0 10) (Gen.choice [ GHC.FunBind <$> located genVarName <*> (GHC.MG <$> located (Gen.list (Range.singleton 1) (located $ GHC.Match <$> pure GHC.NonFunBindMatch <*> Gen.list (Range.linear 1 10) (located genPat) <*> pure Nothing <*> grhsss localBinds)) <*> pure [] <*> pure GHC.PlaceHolder <*> pure FromSource) <*> pure WpHole <*> pure GHC.PlaceHolder <*> pure []
                                                                                                , GHC.PatBind <$> located genPat <*> grhsss localBinds <*> pure GHC.PlaceHolder <*> pure GHC.PlaceHolder <*> pure ([], [])
                                                                                                ]))
  ]

  where

    grhsss localBinds =
      GHC.GRHSs <$> Gen.list (Range.singleton 1) (located genGRHS) <*> located (Gen.choice [ pure GHC.EmptyLocalBinds
                                                                                           , GHC.HsValBinds <$> (GHC.ValBindsIn <$> pure localBinds <*> pure [])
                                                                                           ])


genPat :: Gen (GHC.Pat RdrName.RdrName)
genPat =
  Gen.recursive Gen.choice [ pure $ GHC.WildPat GHC.PlaceHolder
                           , GHC.VarPat <$> located genVarName
                           ] [ GHC.ListPat <$> Gen.list (Range.linear 1 10) (located genPat) <*> pure GHC.PlaceHolder <*> pure Nothing
                             , Gen.subterm2 genPat genPat $ (\l r ->
                               GHC.ConPatIn (GHC.L GHC.noSrcSpan (RdrName.mkVarUnqual . FastString.fsLit $ ":")) (GHC.InfixCon (GHC.L GHC.noSrcSpan l) (GHC.L GHC.noSrcSpan r)))
                             , GHC.ConPatIn <$> located genTypeName <*> (GHC.RecCon <$> (GHC.HsRecFields <$> Gen.list (Range.linear 0 3) (located (GHC.HsRecField <$> located (GHC.FieldOcc <$> located genVarName <*> pure GHC.PlaceHolder) <*> located genPat <*> Gen.bool)) <*> Gen.maybe (Gen.integral (Range.linear 0 2))))
                             , Gen.subtermM genPat $ (\p ->
                               GHC.AsPat <$> located genVarName <*> located (pure p))
                             ]


genGRHS :: Gen (GHC.GRHS RdrName.RdrName (GHC.Located (GHC.HsExpr RdrName.RdrName)))
genGRHS =
  GHC.GRHS <$> Gen.list (Range.linear 1 2) genBodyStmt <*> located genExpr


syntaxExpr =
  GHC.SyntaxExpr {syn_expr = GHC.EWildPat, syn_arg_wraps = [], syn_res_wrap = WpHole}


genBodyStmt :: Gen (GHC.ExprLStmt RdrName.RdrName)
genBodyStmt =
  located $ GHC.BodyStmt <$> located (Gen.filter (not . isHsLet) genExpr) <*> pure syntaxExpr <*> pure syntaxExpr <*> pure GHC.PlaceHolder

  where

    isHsLet GHC.HsLet {} =
      True
    isHsLet _ =
      False


genStmt :: Gen (GHC.ExprLStmt RdrName.RdrName)
genStmt =
  Gen.choice [ genBodyStmt
             , located $ GHC.BindStmt <$> located genPat <*> located genExpr <*> pure syntaxExpr <*> pure syntaxExpr <*> pure GHC.PlaceHolder
             , located $ GHC.LetStmt <$> located genLocalBinds
             ]


genLocalBinds =
  GHC.HsValBinds <$> (GHC.ValBindsIn <$> fmap Bag.listToBag (Gen.list (Range.linear 1 10) (located genBind)) <*> pure [])


genExpr :: Gen (GHC.HsExpr RdrName.RdrName)
genExpr =
  Gen.recursive Gen.choice [ GHC.HsVar <$> located genVarName
                           , GHC.HsOverLit <$> (GHC.OverLit <$> (Gen.choice [ fmap (\i ->
                             GHC.HsIntegral (show i) i) $ Gen.integral (Range.linear 0 100)
                           ]) <*> pure GHC.PlaceHolder <*> pure GHC.EWildPat <*> pure GHC.PlaceHolder)
                           , GHC.HsLit <$> Gen.choice [ fmap (\s ->
                             GHC.HsString (show s) (FastString.fsLit s)) $ Gen.string (Range.linear 0 100) Gen.alphaNum
                           ]
                           ] [ Gen.subtermM genExpr $ (\expr ->
                               GHC.HsLam <$> genMG (Range.singleton 1) (Range.linear 1 3) (Range.singleton 0) (pure expr))
                             , Gen.subterm2 genExpr genExpr (\a b ->
                               GHC.HsApp (GHC.L GHC.noSrcSpan a) (GHC.L GHC.noSrcSpan b))
                             , Gen.subtermM2 genExpr genExpr (\l r ->
                               GHC.OpApp <$> located (pure l) <*> located (GHC.HsVar <$> located genVarName) <*> pure GHC.PlaceHolder <*> located (pure r))
                             , Gen.subterm genExpr (GHC.HsPar . GHC.L GHC.noSrcSpan)
                             , Gen.shrink (\expr ->
                               case expr of
                                 GHC.HsCase _ (GHC.MG (GHC.L _ alts) _ _ _) ->
                                   do
                                     GHC.L _ (GHC.Match _ _ _ (GHC.GRHSs bnds _)) <-
                                       alts

                                     GHC.L _ (GHC.GRHS _ (GHC.L _ body)) <-
                                       bnds

                                     return body

                                 _ ->
                                   []) $ Gen.subtermM genExpr $ (\e ->
                               GHC.HsCase <$> located (pure e) <*> genMG (Range.linear 1 3) (Range.singleton 1) (Range.linear 0 2) genExpr)
                             , Gen.subterm3 genExpr genExpr genExpr $ (\a b c ->
                               GHC.HsIf Nothing (GHC.L GHC.noSrcSpan a) (GHC.L GHC.noSrcSpan b) (GHC.L GHC.noSrcSpan c))
                             , Gen.subtermM genExpr $ (\body ->
                               GHC.HsLet <$> located genLocalBinds <*> located (pure body))
                             , GHC.HsDo <$> pure GHC.DoExpr <*> located (Gen.list (Range.linear 1 10) genStmt) <*> pure GHC.PlaceHolder
                             , GHC.ExplicitTuple <$> Gen.list (Range.linear 1 10) (located (GHC.Present <$> located genExpr)) <*> pure Boxed
                             , GHC.ExplicitList <$> pure GHC.PlaceHolder <*> pure Nothing <*> Gen.list (Range.linear 1 10) (located genExpr)
                             , GHC.RecordCon <$> located genTypeName <*> pure GHC.PlaceHolder <*> pure GHC.EWildPat <*> (GHC.HsRecFields <$> Gen.list (Range.linear 1 3) (located (GHC.HsRecField <$> located (GHC.FieldOcc <$> located genVarName <*> pure GHC.PlaceHolder) <*> located genExpr <*> Gen.bool)) <*> Gen.maybe (Gen.integral (Range.linear 0 100)))
                             , Gen.subtermM genExpr $ (\e ->
                               GHC.ExprWithTySig <$> located (pure e) <*> (GHC.HsIB <$> pure GHC.PlaceHolder <*> (GHC.HsWC <$> pure GHC.PlaceHolder <*> pure Nothing <*> located genHsType)))
                             , Gen.subtermM genExpr $ (\expr ->
                               GHC.RecordUpd <$> located (pure expr) <*> Gen.list (Range.linear 1 3) (located $ GHC.HsRecField <$> located (GHC.Unambiguous <$> located genVarName <*> pure GHC.PlaceHolder) <*> located genExpr <*> Gen.bool) <*> pure GHC.PlaceHolder <*> pure GHC.PlaceHolder <*> pure GHC.PlaceHolder <*> pure GHC.PlaceHolder)
                             ]


genMG :: Range Int -> Range Int -> Range Int -> Gen a -> Gen (GHC.MatchGroup RdrName.RdrName (GHC.Located a))
genMG matches patterns guards genExpr =
  GHC.MG <$> located (Gen.list matches (located (GHC.Match <$> pure GHC.NonFunBindMatch <*> Gen.list patterns (located genPat) <*> pure Nothing <*> (GHC.GRHSs <$> (pure <$> located (GHC.GRHS <$> Gen.list guards genBodyStmt <*> located genExpr)) <*> located (pure GHC.EmptyLocalBinds))))) <*> pure [] <*> pure GHC.PlaceHolder <*> pure FromSource


located :: Monad m => m a -> m (GHC.Located a)
located =
  fmap (GHC.L srcSpan)


srcSpan :: GHC.SrcSpan
srcSpan =
  GHC.noSrcSpan


genImportDecl :: Gen (GHC.ImportDecl RdrName.RdrName)
genImportDecl =
  do
    ideclSourceSrc <-
      pure Nothing

    ideclName <-
      located genModuleName

    ideclPkgQual <-
      pure Nothing

    ideclSource <-
      Gen.bool

    ideclSafe <-
      Gen.bool

    ideclQualified <-
      Gen.bool

    ideclImplicit <-
      Gen.bool

    ideclHiding <-
      Gen.maybe ((,) <$> Gen.bool <*> located (Gen.list (Range.linear 0 10) (located genIE)))

    ideclAs <-
      Gen.maybe genModuleName

    pure GHC.ImportDecl {..}


genIE :: Gen (GHC.IE RdrName.RdrName)
genIE =
  Gen.choice [ GHC.IEVar <$> located genAnyName
             , GHC.IEThingAbs <$> located genAnyName
             , GHC.IEThingAll <$> located genTypeName
             , GHC.IEThingWith <$> located genTypeName <*> pure GHC.NoIEWildcard <*> Gen.list (Range.linear 0 3) (located genVarName) <*> pure []
             ]


genModuleName :: Gen GHC.ModuleName
genModuleName =
  fmap GHC.mkModuleName $ (:) <$> Gen.upper <*> Gen.string (Range.linear 0 10) Gen.alphaNum


typeName :: Gen [Char]
typeName =
  (:) <$> Gen.upper <*> Gen.string (Range.linear 0 10) Gen.alphaNum


genTypeName :: Gen RdrName.RdrName
genTypeName =
  fmap (RdrName.mkVarUnqual . FastString.fsLit) typeName


genTyVar :: Gen RdrName.RdrName
genTyVar =
  fmap (RdrName.mkVarUnqual . FastString.fsLit) $ Gen.filter (\name ->
    not (name `elem` ["do", "if", "of", "in", "let"
                     ])) $ ((:) <$> Gen.lower <*> Gen.string (Range.linear 0 10) Gen.alphaNum)


genVarName :: Gen RdrName.RdrName
genVarName =
  Gen.choice [genTyVar, fmap (RdrName.mkVarUnqual . FastString.fsLit) $ pure "+"
             ]


genAnyName =
  Gen.choice [genVarName, genTypeName]

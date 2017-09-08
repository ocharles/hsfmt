{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

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
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  checkParallel $$(discover)
  return ()

prop_moduleRoundtrip :: Property
prop_moduleRoundtrip =
  property $ do
    ShowModule mod <- forAll (fmap ShowModule genModule)
    parse <- liftIO (parseModuleFromString "input.hs" (show (pretty mod)))
    case parse of
      Left e -> fail (show e)
      Right (_, parsed) -> do
        footnote
          (ppDiff $
           getGroupedDiff
             (lines $ show (pretty mod))
             (lines $ show (pretty parsed)))
        assert $ show (pretty mod) == show (pretty parsed)

newtype ShowModule =
  ShowModule (GHC.HsModule GHC.RdrName)

instance Show ShowModule where
  show (ShowModule mod) = show (pretty mod)

class SynEq a where
  synEq :: a -> a -> Bool
  default synEq :: Eq a => a -> a -> Bool
  synEq = (==)

instance SynEq a => SynEq (GHC.HsModule a) where
  synEq a b =
    GHC.hsmodName a `synEq` GHC.hsmodName b &&
    GHC.hsmodImports a `synEq` GHC.hsmodImports b &&
    GHC.hsmodDecls a `synEq` GHC.hsmodDecls b

instance SynEq a => SynEq (GHC.HsDecl a) where
  synEq (GHC.TyClD a) (GHC.TyClD b) = a `synEq` b
  synEq (GHC.InstD a) (GHC.InstD b) = True
  synEq (GHC.ValD a) (GHC.ValD b) = True
  synEq (GHC.SigD a) (GHC.SigD b) = True

instance SynEq a => SynEq (GHC.TyClDecl a) where
  synEq a@GHC.ClassDecl{} b@GHC.ClassDecl{} = True
  synEq a@GHC.DataDecl{} b@GHC.DataDecl{} = True
  synEq a@GHC.SynDecl{} b@GHC.SynDecl{} = True

instance SynEq a => SynEq [a] where
  synEq (a:as) (b:bs) = a `synEq` b && as `synEq` bs
  synEq [] [] = True
  synEq _ _ = False

instance SynEq a => SynEq (GHC.ImportDecl a) where
  synEq a b =
    GHC.ideclName a `synEq` GHC.ideclName b
    && GHC.ideclPkgQual a `synEq` GHC.ideclPkgQual b
    && GHC.ideclSource a `synEq` GHC.ideclSource b
    && GHC.ideclQualified a `synEq` GHC.ideclQualified b
    && GHC.ideclHiding a `synEq` GHC.ideclHiding b
    && GHC.ideclAs a `synEq` GHC.ideclAs b

instance SynEq Bool

instance (SynEq a, SynEq b) => SynEq (a, b) where
  synEq (a, a') (b, b') = a `synEq` b && a' `synEq` b'

instance (SynEq a) => SynEq (GHC.IE a) where
  synEq a b = GHC.ieNames a `synEq` GHC.ieNames b


  -- synEq _ _ = False

instance SynEq BasicTypes.StringLiteral

instance SynEq a => SynEq (Maybe a) where
  synEq Nothing Nothing = True
  synEq (Just a) (Just b) = a `synEq` b
  synEq _ _ = False

instance SynEq a => SynEq (GHC.Located a) where
  synEq (GHC.L _ a) (GHC.L _ b) = synEq a b

instance SynEq GHC.ModuleName where

instance SynEq GHC.RdrName where
  synEq a b = show (pretty a) `synEq` show (pretty b)

instance SynEq Char where

genModule = do
  hsmodName <- Gen.maybe . located $ genModuleName
  hsmodImports <- Gen.list (Range.linear 0 10) (located genImportDecl)
  hsmodExports <- Gen.maybe . located $ Gen.list (Range.linear 0 10) (located genIE)
  hsmodDecls <- Gen.list (Range.linear 0 10) (located genDecl)
  hsmodDeprecMessage <- pure Nothing
  hsmodHaddockModHeader <- pure Nothing
  pure GHC.HsModule { ..}

genDecl =
  Gen.choice
    [ GHC.TyClD <$> genTyClDecl
    , GHC.InstD <$> genInstDecl
    , GHC.SigD <$> genSig
    -- , GHC.SpliceD <$> genSpliceDecl
    , GHC.ValD <$> genBind
    ]

genSpliceDecl =
  GHC.SpliceDecl <$> located genHsSplice <*>
  Gen.element [GHC.ExplicitSplice, GHC.ImplicitSplice]

genHsSplice =
  Gen.choice
    [
    -- [ GHC.HsQuasiQuote <$> genName <*> genName <*> pure GHC.noSrcSpan <*>
    --   (FastString.fsLit <$> Gen.string (Range.linear 1 1000) Gen.alphaNum)
     GHC.HsUntypedSplice <$> genName <*> located genExpr
    ]

genInstDecl =
  Gen.choice [ GHC.ClsInstD <$> genClsInstDecl ]

genClsInstDecl =
  GHC.ClsInstDecl <$> genLHsSigType <*> genBinds <*>
  pure [] <*>
  pure []<*>
  pure []<*>
  pure Nothing

genLDataFamInstDecl = located genDataFamInstDecl

genDataFamInstDecl =
  GHC.DataFamInstDecl <$> located genName <*> genHsTyPats <*> genHsDataDefn <*>
  pure GHC.PlaceHolder

genHsTyPats =
  GHC.HsIB <$> pure GHC.PlaceHolder <*>
  (Gen.list (Range.linear 0 10) (located genHsType))

genHsImplicitBndrs = GHC.HsIB <$> pure GHC.PlaceHolder <*> located genHsType

-- genLHsBinds =
--   Bag.listToBag <$> genBinds

-- genLSig = located genSig

genSig =
  Gen.choice
    [ GHC.TypeSig <$> Gen.list (Range.linear 1 10) (located genName) <*>
      (GHC.HsIB <$> pure GHC.PlaceHolder <*>
       (GHC.HsWC <$> pure GHC.PlaceHolder <*> pure Nothing <*> located genHsType))
    ]

genTyClDecl = Gen.choice [dataDecl, synDecl, classDecl]
  where
    dataDecl =
      GHC.DataDecl <$> located genTypeName <*> genLHsQTyVars <*> genHsDataDefn <*>
      pure GHC.PlaceHolder <*>
      pure GHC.PlaceHolder
    synDecl =
      GHC.SynDecl <$> located genTypeName <*> genLHsQTyVars <*>
      located genHsType <*>
      pure GHC.PlaceHolder
    classDecl =
      GHC.ClassDecl <$> located (pure []) <*> located genTypeName <*> genLHsQTyVars <*>
      pure [] <*>
      pure [] <*>
      genBinds <*>
      pure [] <*>
      pure [] <*>
      pure [] <*>
      pure GHC.PlaceHolder

genLHsQTyVars =
  GHC.HsQTvs <$> pure GHC.PlaceHolder <*>
  Gen.list (Range.linear 0 10) (located genHsTyVarBndr) <*>
  pure GHC.PlaceHolder

genHsTyVarBndr = Gen.choice [GHC.UserTyVar <$> located genName]

genHsDataDefn = do
  newOrData <- genNewOrData
  GHC.HsDataDefn <$> pure newOrData <*>
    located (Gen.list (Range.linear 0 10) (located genHsType)) <*>
    pure Nothing <*>
    Gen.maybe (located genHsKind) <*>
    Gen.list
      (if newOrData == GHC.NewType
         then Range.singleton 1
         else Range.linear 0 10)
      (located genConDecl) <*>
    genHsDeriving

genHsDeriving = Gen.maybe (located (Gen.list (Range.linear 0 10) genLHsSigType))

genLHsSigType = genHsImplicitBndrs

genConDecl =
  Gen.choice
    [ GHC.ConDeclH98 <$> located genTypeName <*> Gen.maybe genLHsQTyVars <*>
      Gen.maybe (located (Gen.list (Range.linear 0 10) (located genHsType))) <*>
      genHsConDeclDetails <*>
      pure Nothing
    ]

genHsConDeclDetails =
  Gen.choice
    [ GHC.PrefixCon <$> Gen.list (Range.linear 0 10) (located genHsType)
    , GHC.RecCon <$>
      located (Gen.list (Range.linear 0 10) (located genConDeclField))
    ]

genConDeclField =
  GHC.ConDeclField <$>
  Gen.list
    (Range.linear 1 10)
    (located (GHC.FieldOcc <$> located genName <*> pure GHC.PlaceHolder)) <*>
  located genHsType <*>
  pure Nothing

genHsKind = genHsType

genHsType = Gen.choice [GHC.HsTyVar <$> located genName]

genNewOrData = Gen.choice [ pure GHC.NewType, pure GHC.DataType ]

genBind =
  Gen.choice
    [ GHC.VarBind <$> genName <*> located genExpr <*> Gen.bool
    , GHC.FunBind <$> located genName <*>
      (GHC.MG <$>
       located
         (Gen.list
            (Range.singleton 1)
            (located $
             GHC.Match <$> pure undefined <*>
             Gen.list (Range.linear 0 10) (located genPat) <*>
             pure Nothing <*>
             grhsss)) <*>
       pure [] <*>
       pure GHC.PlaceHolder <*>
       pure undefined) <*>
      pure undefined <*>
      pure GHC.PlaceHolder <*>
      pure []
    , GHC.PatBind <$> located genPat <*> grhsss <*>
      pure GHC.PlaceHolder <*>
      pure GHC.PlaceHolder <*>
      pure ([], [])
    ]

  where
    grhsss =
      GHC.GRHSs <$> Gen.list (Range.linear 1 3) (located genGRHS) <*>
      located
        (Gen.choice
           [ pure GHC.EmptyLocalBinds
           , GHC.HsValBinds <$> (GHC.ValBindsIn <$> genBinds <*> pure [])
           ])

genBinds :: Gen (GHC.LHsBinds RdrName.RdrName)
genBinds =
  Gen.recursive
    Gen.choice
    [ fmap (Bag.listToBag . map (GHC.L GHC.noSrcSpan)) $
      Gen.list
        (Range.singleton 1)
        (GHC.VarBind <$> genName <*> located genExpr <*> Gen.bool)
    ]
    [ Gen.subtermM
        genBinds
        (\localBinds ->
           fmap (Bag.listToBag . map (GHC.L GHC.noSrcSpan)) $
           Gen.list
             (Range.linear 0 10)
             (Gen.choice
                [ GHC.FunBind <$> located genName <*>
                  (GHC.MG <$>
                   located
                     (Gen.list
                        (Range.singleton 1)
                        (located $
                         GHC.Match <$> pure undefined <*>
                         Gen.list (Range.linear 1 10) (located genPat) <*>
                         pure Nothing <*>
                         grhsss localBinds)) <*>
                   pure [] <*>
                   pure GHC.PlaceHolder <*>
                   pure undefined) <*>
                  pure undefined <*>
                  pure GHC.PlaceHolder <*>
                  pure []
                , GHC.PatBind <$> located genPat <*> grhsss localBinds <*>
                  pure GHC.PlaceHolder <*>
                  pure GHC.PlaceHolder <*>
                  pure ([], [])
                ]))
    ]
  where
    grhsss localBinds =
      GHC.GRHSs <$> Gen.list (Range.singleton 1) (located genGRHS) <*>
      located
        (Gen.choice
           [ pure GHC.EmptyLocalBinds
           , GHC.HsValBinds <$> (GHC.ValBindsIn <$> pure localBinds <*> pure [])
           ])

genPat =
  Gen.recursive
    Gen.choice
    [pure $ GHC.WildPat GHC.PlaceHolder, GHC.VarPat <$> located genName]
    [ GHC.ListPat <$> Gen.list (Range.linear 1 10) (located genPat) <*>
      pure GHC.PlaceHolder <*>
      pure Nothing
    , Gen.subterm2 genPat genPat $ \l r ->
        GHC.ConPatIn
          (GHC.L GHC.noSrcSpan (RdrName.mkVarUnqual . FastString.fsLit $ ":"))
          (GHC.InfixCon (GHC.L GHC.noSrcSpan l) (GHC.L GHC.noSrcSpan r))
    ]


genGRHS =
  GHC.GRHS <$> Gen.list (Range.linear 1 2) genStmt <*> located genExpr

genStmt :: Gen (GHC.ExprLStmt RdrName.RdrName)
genStmt =
  located $
  Gen.choice
    [ GHC.BodyStmt <$> located (Gen.filter (not . isHsLet) genExpr) <*>
      pure undefined <*>
      pure undefined <*>
      pure GHC.PlaceHolder
    ]
  where
    isHsLet GHC.HsLet {} = True
    isHsLet _ = False

genExpr :: Gen (GHC.HsExpr RdrName.RdrName)
genExpr =
  Gen.recursive
    Gen.choice
    [ GHC.HsVar <$> located genName
    , GHC.HsOverLit <$>
      (GHC.OverLit <$>
       (Gen.choice
          [ fmap (\i -> GHC.HsIntegral (show i) i) $
            Gen.integral (Range.linear 0 100)
          ]) <*>
       pure GHC.PlaceHolder <*>
       pure undefined <*>
       pure GHC.PlaceHolder)
    , GHC.HsLit <$>
      Gen.choice
        [ fmap (\s -> GHC.HsString (show s) (FastString.fsLit s)) $
          Gen.string (Range.linear 0 100) Gen.alphaNum
        ]
    ]
    [ Gen.subtermM genExpr $ \expr ->
        GHC.HsLam <$>
        genMG
          (Range.singleton 1)
          (Range.linear 1 3)
          (Range.singleton 0)
          (pure expr)
    , Gen.subterm2
        genExpr
        genExpr
        (\a b -> GHC.HsApp (GHC.L GHC.noSrcSpan a) (GHC.L GHC.noSrcSpan b))
    , Gen.subtermM2
        genExpr
        genExpr
        (\l r ->
           GHC.OpApp <$> located (pure l) <*>
           located (GHC.HsVar <$> located genName) <*>
           pure GHC.PlaceHolder <*>
           located (pure r))
    , Gen.subterm genExpr (GHC.HsPar . GHC.L GHC.noSrcSpan)
    , Gen.subtermM genExpr $ \e ->
        GHC.HsCase <$> located (pure e) <*>
        genMG (Range.singleton 1) (Range.singleton 1) (Range.linear 0 2) genExpr
    , Gen.subterm3 genExpr genExpr genExpr $ \a b c ->
        GHC.HsIf
          Nothing
          (GHC.L GHC.noSrcSpan a)
          (GHC.L GHC.noSrcSpan b)
          (GHC.L GHC.noSrcSpan c)
    , Gen.subtermM genExpr $ \body ->
        GHC.HsLet <$>
        located
          (GHC.HsValBinds <$>
           (GHC.ValBindsIn <$>
            fmap Bag.listToBag (Gen.list (Range.linear 1 10) (located genBind)) <*>
            pure [])) <*>
        located (pure body)
    , GHC.HsDo <$> pure GHC.DoExpr <*>
      located (Gen.list (Range.linear 1 10) genStmt) <*>
      pure GHC.PlaceHolder
    , GHC.ExplicitTuple <$>
      Gen.list (Range.linear 1 10) (located (GHC.Present <$> located genExpr)) <*>
      pure undefined
    , GHC.ExplicitList GHC.PlaceHolder Nothing <$>
      Gen.list (Range.linear 1 10) (located genExpr)
    ]

genMG matches patterns guards genExpr =
  GHC.MG <$>
  located
    (Gen.list
       matches
       (located
          (GHC.Match <$> pure undefined <*> Gen.list patterns (located genPat) <*>
           pure Nothing <*>
           (GHC.GRHSs <$>
            (pure <$>
             located
               (GHC.GRHS <$> Gen.list guards genStmt <*>
                located genExpr)) <*>
            located (pure GHC.EmptyLocalBinds))))) <*>
  pure [] <*>
  pure GHC.PlaceHolder <*>
  pure undefined

located :: Monad m => m a -> m (GHC.Located a)
located = fmap (GHC.L srcSpan)

srcSpan = GHC.noSrcSpan

genImportDecl = do
  ideclSourceSrc <- pure Nothing
  ideclName <- located genModuleName
  ideclPkgQual <- pure Nothing
  ideclSource <- Gen.bool
  ideclSafe <- Gen.bool
  ideclQualified <- Gen.bool
  ideclImplicit <- Gen.bool
  ideclHiding <-
    Gen.maybe
      ((,) <$> Gen.bool <*>
       located (Gen.list (Range.linear 0 10) (located genIE)))
  ideclAs <- Gen.maybe genModuleName
  pure GHC.ImportDecl {..}

-- genIE :: (Monad m, GenName name) => Gen m (GHC.IE name)
genIE = Gen.choice [ GHC.IEVar <$> located genName ]

-- genModuleName :: Monad m => Gen m GHC.ModuleName
genModuleName =
  fmap GHC.mkModuleName $
  (:) <$> Gen.upper <*> Gen.string (Range.linear 0 10) Gen.alphaNum

genTypeName =
    fmap (RdrName.mkVarUnqual . FastString.fsLit) $
    ((:) <$> Gen.upper <*> Gen.string (Range.linear 0 10) Gen.alphaNum)

class GenName a where
  genName :: Gen a

instance GenName GHC.RdrName where
  genName =
    fmap (RdrName.mkVarUnqual . FastString.fsLit) $
    Gen.filter (\name -> not (name `elem` ["do", "if", "of", "in", "let"])) $
    ((:) <$> Gen.lower <*> Gen.string (Range.linear 0 10) Gen.alphaNum)

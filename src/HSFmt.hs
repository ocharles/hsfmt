{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HSFmt ( prettyPrintFile ) where

import Data.Maybe
import Data.Text.Prettyprint.Doc
import Data.Foldable (toList)
import FastString
import GHC hiding (parseModule)
import Language.Haskell.GHC.ExactPrint ( Annotation(..)
                                       , Anns
                                       , AnnKey(..)
                                       , parseModule )
import Module
import qualified Name as GHC
import OccName
import RdrName



groupDecls :: Eq id => [LHsDecl id] -> [[LHsDecl id]]
groupDecls []  =
  []
groupDecls (x@(L _ (SigD (TypeSig names _))) : xs) =
  let
    (binds, rest) =
      splitNames (fmap unLoc names) xs

  in
    (x : binds) : groupDecls rest
groupDecls (x : xs) =
  [x] : groupDecls xs


splitNames :: Eq id => [id] -> [LHsDecl id] -> ( [LHsDecl id]
                                               , [LHsDecl id] )
splitNames names []  =
  ([], [])
splitNames names (x : xs) =
  case unLoc x of
    ValD (FunBind {..}) ->
      if unLoc fun_id `elem` names then
        let
          (binds, rest) =
            splitNames names xs

        in
          (x : binds, rest)
      else
        ([], x : xs)

    _ ->
      ([], x : xs)


prettyPrintFile :: FilePath -> IO String
prettyPrintFile path =
  do
    out <-
      parseModule path

    case out of
      Left e ->
        error (show e)

      Right (anns, parsed) ->
        return (show (pretty parsed))


instance Pretty ParsedSource where
  pretty (L _loc parsedSource) =
    pretty parsedSource


instance Pretty (HsModule RdrName) where
  pretty HsModule {hsmodName, hsmodExports, hsmodImports, hsmodDecls} =
    concatWith (\x y ->
      x <> hardline <> hardline <> hardline <> hardline <> y) [concatWith (\x y ->
      x <> hardline <> hardline <> y) $ catMaybes [flip fmap hsmodName $ (\moduleName ->
      hsep $ catMaybes $ [Just "module", Just $ pretty moduleName, fmap pretty hsmodExports, Just "where"]), case hsmodImports of
                                                                                                               []  ->
                                                                                                                 Nothing

                                                                                                               _ ->
                                                                                                                 Just (pretty hsmodImports)], pretty hsmodDecls]


instance Pretty (Located (HsDecl RdrName)) where
  pretty (L _loc decl) =
    pretty decl

  prettyList =
    concatWith (\x y ->
      x <> hardline <> hardline <> hardline <> y) . map (hardVsep . map pretty) . groupDecls


instance Pretty (HsDecl RdrName) where
  pretty (TyClD a) =
    pretty a
  pretty (InstD inst) =
    pretty inst
  pretty (ValD b) =
    prettyBind equals b
  pretty (SigD a) =
    pretty a
  pretty (SpliceD a) =
    pretty a


instance Pretty (SpliceDecl RdrName) where
  pretty (SpliceDecl a _) =
    pretty a


instance Pretty (Located (HsSplice RdrName)) where
  pretty (L _loc a) =
    pretty a


instance Pretty (TyClDecl RdrName) where
  pretty SynDecl {tcdLName, tcdRhs} =
    "type" <+> pretty tcdLName <+> equals <+> pretty tcdRhs
  pretty DataDecl {..} =
    let
      HsDataDefn {..} =
        tcdDataDefn

    in
      nest 2 $ (case dd_ND of
                  NewType  ->
                    "newtype"

                  DataType  ->
                    "data") <+> pretty tcdLName <> (case hsq_explicit tcdTyVars of
                                                      []  ->
                                                        mempty

                                                      vars ->
                                                        space <> hsep (map pretty vars)) <> (case dd_cons of
                                                                                               []  ->
                                                                                                 mempty

                                                                                               cons ->
                                                                                                 space <> equals <+> pretty dd_cons) <> foldMap (\a ->
        space <> "deriving" <+> pretty a) dd_derivs
  pretty ClassDecl {..} =
    "class" <+> pretty tcdLName <+> pretty tcdTyVars <+> "where" <> hardline <> indent 2 (concatWith (\x y ->
      x <> hardline <> hardline <> y) $ map (prettyBind equals . unLoc) (toList tcdMeths) ++ map pretty tcdSigs)


instance Pretty (LHsQTyVars RdrName) where
  pretty HsQTvs {..} =
    hsep $ map pretty hsq_explicit


instance Pretty (LHsTyVarBndr RdrName) where
  pretty (L _loc a) =
    pretty a


instance Pretty (HsTyVarBndr RdrName) where
  pretty (UserTyVar n) =
    pretty n


instance Pretty (LSig RdrName) where
  pretty (L _loc a) =
    pretty a


instance Pretty (Located [LHsSigType RdrName]) where
  pretty (L _loc a) =
    pretty a


instance Pretty (Located (ConDecl RdrName)) where
  pretty (L _loc a) =
    pretty a

  prettyList =
    hsep . punctuate "|" . map pretty


instance Pretty (ConDecl RdrName) where
  pretty ConDeclH98 {con_name, con_details} =
    pretty con_name <+> (case con_details of
                           RecCon xs ->
                             align (braces (pretty xs))

                           PrefixCon args ->
                             hsep (map (parTy . unLoc) args))

    where

      parTy (t@HsForAllTy {}) =
        parens (pretty t)
      parTy (t@HsQualTy {}) =
        parens (pretty t)
      parTy (t@HsAppsTy {}) =
        parens (pretty t)
      parTy (t@HsAppTy {}) =
        parens (pretty t)
      parTy (t@HsFunTy {}) =
        parens (pretty t)
      parTy t =
        pretty t


instance Pretty (Located [LConDeclField RdrName]) where
  pretty (L _loc a) =
    pretty a


instance Pretty (Located (ConDeclField RdrName)) where
  pretty (L _loc a) =
    pretty a

  prettyList =
    hsep . punctuate comma . map pretty


instance Pretty (ConDeclField RdrName) where
  pretty ConDeclField {cd_fld_names, cd_fld_type} =
    pretty cd_fld_names <+> "::" <+> pretty cd_fld_type


instance Pretty (Located (FieldOcc RdrName)) where
  pretty (L _loc a) =
    pretty a

  prettyList =
    hsep . punctuate comma . map pretty


instance Pretty (FieldOcc RdrName) where
  pretty FieldOcc {rdrNameFieldOcc} =
    pretty rdrNameFieldOcc


instance Pretty (InstDecl RdrName) where
  pretty ClsInstD {cid_inst} =
    pretty cid_inst


instance Pretty (ClsInstDecl RdrName) where
  pretty ClsInstDecl {cid_poly_ty, cid_binds} =
    "instance" <+> pretty cid_poly_ty <+> "where" <> hardline <> indent 2 (concatWith (\x y ->
      x <> hardline <> hardline <> y) (map (prettyBind equals . unLoc) (toList cid_binds)))


instance Pretty (Located (StmtLR RdrName RdrName (LHsExpr RdrName))) where
  pretty (L _loc a) =
    pretty a

  prettyList =
    concatWith (\x y ->
      x <> hardline <> hardline <> y) . map pretty


instance Pretty (StmtLR RdrName RdrName (LHsExpr RdrName)) where
  pretty (BindStmt p bod _ _ _) =
    align $ parPat (unLoc p) <+> "<-" <> hardline <> indent 2 (pretty bod)
  pretty (BodyStmt body _ _ _) =
    pretty body
  pretty (LetStmt binds) =
    "let" <> hardline <+> indent 2 (prettyHsLocalBinds equals (unLoc binds))


instance Pretty (Located (HsExpr RdrName)) where
  pretty (L _loc a) =
    pretty a


instance Pretty (HsOverLit RdrName) where
  pretty OverLit {ol_val} =
    pretty ol_val


instance Pretty OverLitVal where
  pretty (HsIntegral st _) =
    pretty st


parensExpr (expr@HsLam {}) =
  parens (pretty expr)
parensExpr (expr@HsCase {}) =
  parens (pretty expr)
parensExpr (expr@HsIf {}) =
  parens (pretty expr)
parensExpr (expr@HsLet {}) =
  parens (pretty expr)
parensExpr (expr@HsDo {}) =
  parens (pretty expr)
parensExpr (expr@NegApp {}) =
  parens (pretty expr)
parensExpr a =
  pretty a


instance Pretty (HsExpr RdrName) where
  pretty (HsVar id_) =
    pretty id_
  pretty (HsOverLit a) =
    pretty a
  pretty (HsLit lit) =
    pretty lit
  pretty (HsLam mg) =
    "\\" <> prettyMatchGroup "->" mg
  pretty (HsApp a b) =
    parensExpr (unLoc a) <+> parensExpr (unLoc b)
  pretty (OpApp (L _ a) (L _ (HsVar op)) _ (L _ b))
    | HSFmt.isSymOcc op = parensExpr a <+> prettyName (unLoc op) <+> parensExpr b
    | otherwise = parensExpr a <+> "`" <> pretty op <> "`" <+> parensExpr b
  pretty (OpApp a other _ b) =
    error "OpApp with a non-HsVar operator"
  pretty (NegApp a _) =
    "-" <> pretty a
  pretty (HsPar expr) =
    parens (pretty expr)
  pretty (ExplicitTuple args _) =
    parens $ hsep $ punctuate comma (map pretty args)
  pretty (HsCase expr MG {mg_alts}) =
    align $ "case" <+> pretty expr <+> "of" <> hardline <> indent 2 (concatWith (\x y ->
      x <> hardline <> hardline <> y) (map (prettyMatch "->" . unLoc) (unLoc mg_alts)))
  pretty (HsIf _ a b c) =
    align $ "if" <+> pretty a <+> "then" <> hardline <> indent 2 (pretty b) <> hardline <> "else" <> hardline <> indent 2 (pretty c)
  pretty (HsLet binds expr) =
    align $ "let" <> hardline <> indent 2 (prettyHsLocalBinds equals (unLoc binds)) <> hardline <> hardline <> "in" <> hardline <> indent 2 (pretty expr)
  pretty (HsDo _ exprs _) =
    align $ "do" <> hardline <> indent 2 (concatWith (\x y ->
      x <> hardline <> hardline <> y) (map pretty (unLoc exprs)))
  pretty (ExplicitList _ _ exprs) =
    brackets $ hsep $ punctuate comma $ map pretty exprs
  pretty RecordCon {rcon_con_name, rcon_flds} =
    pretty rcon_con_name <+> pretty rcon_flds
  pretty (HsSpliceE a) =
    pretty a


instance Pretty (HsRecordBinds RdrName) where
  pretty HsRecFields {rec_flds, rec_dotdot} =
    braces (hsep . punctuate comma $ map pretty rec_flds ++ (case rec_dotdot of
                                                               Nothing  ->
                                                                 []

                                                               Just _ ->
                                                                 [".."]))


instance Pretty (LHsRecField RdrName (LHsExpr RdrName)) where
  pretty (L _loc a) =
    pretty a


instance Pretty (HsRecField RdrName (LHsExpr RdrName)) where
  pretty HsRecField {hsRecFieldLbl, hsRecFieldArg} =
    pretty hsRecFieldLbl <+> equals <+> pretty hsRecFieldArg


instance Pretty (HsSplice RdrName) where
  pretty (HsUntypedSplice id_ expr) =
    pretty expr
  pretty (HsQuasiQuote a b _ src) =
    brackets (prettyInfixName b <> "|" <> column (\n ->
      indent (negate n) (pretty src)) <> "|")
  pretty (HsTypedSplice x expr) =
    "$$" <> parens (pretty expr)


instance Pretty (Located [ExprLStmt RdrName]) where
  pretty (L _loc a) =
    pretty a


instance Pretty (LHsTupArg RdrName) where
  pretty (L _loc a) =
    pretty a


instance Pretty (HsTupArg RdrName) where
  pretty (Present expr) =
    pretty expr


instance Pretty HsLit where
  pretty (HsString src _) =
    pretty src


parPat (a@(ConPatIn _ InfixCon {})) =
  parens (pretty a)
parPat (a@AsPat {}) =
  parens (pretty a)
parPat a =
  pretty a


instance Pretty (Pat RdrName) where
  pretty WildPat {} =
    "_"
  pretty (VarPat name) =
    pretty name
  pretty (AsPat id_ pat) =
    pretty id_ <> "@" <> pretty pat
  pretty (ParPat p) =
    parens (pretty p)
  pretty (TuplePat pats _ _) =
    tupled (map pretty pats)
  pretty (ConPatIn id_ (InfixCon a b)) =
    pretty a <+> prettyName (unLoc id_) <+> pretty b
  pretty (ConPatIn id_ details) =
    pretty id_ <+> pretty details
  pretty (LitPat a) =
    pretty a
  pretty (ListPat pats _ _) =
    brackets (hsep $ punctuate comma $ map pretty pats)


instance Pretty (HsConPatDetails RdrName) where
  pretty (PrefixCon args) =
    hsep (map pretty args)
  pretty (RecCon rec_) =
    pretty rec_
  pretty (InfixCon a b) =
    parens $ pretty a <+> pretty b


instance Pretty (HsRecFields RdrName (LPat RdrName)) where
  pretty HsRecFields {rec_flds, rec_dotdot} =
    braces $ hsep $ punctuate comma $ map pretty rec_flds ++ maybe [] (const [".."]) rec_dotdot


instance Pretty (Located (HsRecField RdrName (LPat RdrName))) where
  pretty (L _loc a) =
    pretty a


instance Pretty (HsRecField RdrName (LPat RdrName)) where
  pretty HsRecField {..} =
    pretty hsRecFieldLbl <> (if hsRecPun then
                               mempty
                             else
                               space <> equals <+> pretty hsRecFieldArg)


instance Pretty (Located (Pat RdrName)) where
  pretty (L _loc a) =
    pretty a


instance Pretty (LHsSigType RdrName) where
  pretty (HsIB _ thing) =
    pretty thing

  prettyList =
    tupled . map pretty


instance Pretty (Sig RdrName) where
  pretty (TypeSig names sig) =
    hsep (punctuate comma (map pretty names)) <+> "::" <+> pretty sig
  pretty (ClassOpSig isDefault b c) =
    (if isDefault then
       "default" <> space
     else
       mempty) <> hsep (punctuate comma (map pretty b)) <+> "::" <+> pretty c


instance Pretty (LHsSigWcType RdrName) where
  pretty HsIB {hsib_body} =
    pretty hsib_body


instance Pretty (LHsWcType RdrName) where
  pretty HsWC {hswc_body} =
    pretty hswc_body


instance Pretty (LHsType RdrName) where
  pretty (L _loc ty) =
    pretty ty

  prettyList =
    tupled . map pretty


instance Pretty (HsType RdrName) where
  pretty HsQualTy {hst_ctxt, hst_body} =
    pretty hst_ctxt <+> "=>" <+> pretty hst_body
  pretty (HsTyVar name) =
    pretty name
  pretty (HsAppsTy apps) =
    pretty apps
  pretty (HsAppTy a b) =
    pretty [a, b]
  pretty (HsFunTy l r) =
    pretty l <+> "->" <+> pretty r
  pretty (HsListTy t) =
    lbracket <> pretty t <> rbracket
  pretty (HsTupleTy tupleSort tys) =
    tupled (map pretty tys)
  pretty (HsParTy a) =
    parens (pretty a)
  pretty (HsForAllTy bndrs t) =
    "forall" <+> hsep (map pretty bndrs) <> dot <+> pretty t


instance Pretty (Located (HsAppType RdrName)) where
  pretty (L _loc appty) =
    pretty appty

  prettyList =
    hsep . map pretty


instance Pretty (HsAppType RdrName) where
  pretty (HsAppPrefix t) =
    pretty t


instance Pretty (LHsContext RdrName) where
  pretty (L _loc [] ) =
    "()"
  pretty (L _loc [L _ t]) =
    pretty t
  pretty (L _loc ts) =
    pretty ts


instance Pretty (Located (ImportDecl RdrName)) where
  pretty (L _loc importDecl) =
    pretty importDecl

  prettyList =
    hardVsep . map pretty


instance Pretty (ImportDecl RdrName) where
  pretty ImportDecl {ideclName, ideclHiding, ideclQualified, ideclAs, ideclSource} =
    hsep $ catMaybes [Just "import", if ideclSource then
                                       Just "{-# SOURCE #-}"
                                     else
                                       Nothing, if ideclQualified then
                                                  Just "qualified"
                                                else
                                                  Nothing, Just (pretty ideclName), flip fmap ideclAs $ (\as ->
      "as" <+> pretty as), flip fmap ideclHiding $ (\( hiding
                                                     , things ) ->
      hsep $ catMaybes [if hiding then
                          Just "hiding"
                        else
                          Nothing, Just (pretty things)])]


instance Pretty (Located ModuleName) where
  pretty (L _loc moduleName) =
    pretty moduleName


instance Pretty ModuleName where
  pretty =
    pretty . moduleNameFS


instance Pretty FastString where
  pretty fs =
    pretty (unpackFS fs)


instance Pretty (Located [LIE RdrName]) where
  pretty (L _loc rdrNames) =
    pretty rdrNames


instance Pretty (Located (IE RdrName)) where
  pretty (L _loc rdrName) =
    pretty rdrName

  prettyList =
    tupled . map pretty


instance Pretty (IE RdrName) where
  pretty (IEVar name) =
    pretty name
  pretty (IEThingAbs name) =
    pretty name
  pretty (IEThingAll name) =
    pretty name <> "(..)"


instance Pretty (Located RdrName) where
  pretty (L _loc rdrName) =
    prettyInfixName rdrName


prettyName :: RdrName -> Doc ann
prettyName n =
  case n of
    Unqual occName ->
      pretty occName

    Qual mod name ->
      pretty mod <> dot <> pretty name

    Orig mod name ->
      pretty mod <> dot <> pretty name

    Exact name ->
      pretty (GHC.nameOccName name)


prettyInfixName :: RdrName -> Doc ann
prettyInfixName n
  | HSFmt.isSymOcc n = lparen <> prettyName n <> rparen
  | otherwise = prettyName n


instance Pretty Module where
  pretty =
    pretty . moduleName


instance Pretty OccName where
  pretty =
    pretty . occNameString


class IsSymOcc a where
  isSymOcc :: a -> Bool


instance IsSymOcc GHC.OccName where
  isSymOcc =
    GHC.isSymOcc


instance IsSymOcc RdrName where
  isSymOcc =
    HSFmt.isSymOcc . rdrNameOcc


instance IsSymOcc b => IsSymOcc (GenLocated a b) where
  isSymOcc =
    HSFmt.isSymOcc . unLoc


hardVsep :: [Doc ann] -> Doc ann
hardVsep =
  concatWith (\x y ->
    x <> hardline <> y)


prettyBind :: Doc ann -> HsBind RdrName -> Doc ann
prettyBind bind hsBind =
  case hsBind of
    FunBind {fun_id, fun_matches} ->
      hardVsep $ map (\alt ->
        pretty fun_id <+> prettyMatch bind (unLoc alt)) (unLoc $ mg_alts fun_matches)

    PatBind {pat_lhs, pat_rhs} ->
      align $ parPat (unLoc pat_lhs) <+> prettyGRHSs bind pat_rhs

    VarBind {var_id, var_rhs} ->
      prettyInfixName var_id <+> bind <> hardline <> indent 2 (pretty var_rhs)


prettyMatch :: Doc ann -> Match RdrName (LHsExpr RdrName) -> Doc ann
prettyMatch bind Match {m_pats, m_grhss} =
  (case m_pats of
     []  ->
       mempty

     _ ->
       hsep (map (parPat . unLoc) m_pats) <> space) <> prettyGRHSs bind m_grhss


prettyGRHSs bind GRHSs {grhssGRHSs, grhssLocalBinds} =
  (case grhssGRHSs of
     [grhs] ->
       prettyGRHS bind (unLoc grhs)

     _ ->
       hardline <> indent 2 (vsep (map (prettyGRHS bind . unLoc) grhssGRHSs))) <> (case unLoc grhssLocalBinds of
                                                                                     EmptyLocalBinds  ->
                                                                                       mempty

                                                                                     _ ->
                                                                                       hardline <> hardline <> indent 2 ("where" <> hardline <> hardline <> indent 2 (prettyHsLocalBinds bind (unLoc grhssLocalBinds))))


prettyGRHS bind (GRHS []  body) =
  bind <> hardline <> indent 2 (pretty body)
prettyGRHS bind (GRHS guards body) =
  "|" <+> hsep (punctuate comma (map pretty guards)) <+> bind <+> pretty body


prettyHsLocalBinds bind (HsValBinds b) =
  prettyHsValBindsLR bind b


prettyHsValBindsLR bind (ValBindsIn bnds _) =
  concatWith (\x y ->
    x <> hardline <> hardline <> y) $ map (prettyBind bind . unLoc) (toList bnds)


prettyMatchGroup bind MG {mg_alts} =
  hardVsep $ map (prettyMatch bind . unLoc) (unLoc mg_alts)

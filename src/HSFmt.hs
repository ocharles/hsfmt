{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module HSFmt (prettyPrintFile) where

import Control.Monad
import qualified Data.Map as Map
import Data.Char
import BasicTypes (fl_text)
import Data.Maybe
import Data.Text.Prettyprint.Doc hiding (list, tupled)
import Data.Text.Prettyprint.Doc.Render.String
import Data.Foldable (toList)
import FastString
import GHC hiding (parseModule)
import Language.Haskell.GHC.ExactPrint ( Annotation(..)
                                       , Anns
                                       , AnnKey(..)
                                       , parseModule
                                       )
import Language.Haskell.GHC.ExactPrint.Types ( annGetConstr
                                             , KeywordId(AnnComment)
                                             , Comment(..)
                                             )
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


splitNames :: Eq id => [id] -> [LHsDecl id] -> ([LHsDecl id], [LHsDecl id])
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


nullWhitespace :: String -> String
nullWhitespace s =
  fromMaybe s (go s)

  where

    go []  =
      Just []
    go (a : as)
      | isSpace a = go as
      | otherwise = Nothing


prettyPrintFile :: FilePath -> IO String
prettyPrintFile path =
  do
    out <-
      parseModule path

    case out of
      Left e ->
        error (show e)

      Right (anns, parsed) ->
        return
          $ concat
              [ case parsed of
                  L srcSpan a ->
                    case AnnKey srcSpan (annGetConstr a) `Map.lookup` anns of
                      Just ann ->
                        unlines $ concatMap yieldComment (annsDP ann)

                      Nothing  ->
                        mzero
              , unlines $ map nullWhitespace $ lines $ renderString
                  $ layoutPretty
                      defaultLayoutOptions
                        { layoutPageWidth = AvailablePerLine 80 1 }
                      (((pretty parsed :: Doc ())))
              ]

  where

    yieldComment ((AnnComment (Comment c _ _)), _) =
      return c
    yieldComment _ =
      mzero


instance Pretty ParsedSource where
  pretty (L _loc parsedSource) =
    pretty parsedSource


instance Pretty (HsModule RdrName) where
  pretty HsModule {hsmodName, hsmodExports, hsmodImports, hsmodDecls} =
    concatWith
      (\x y ->
          x <> hardline <> hardline <> hardline <> hardline <> y)
      [ concatWith
          (\x y ->
              x <> hardline <> hardline <> y)
          $ catMaybes
              [ fmap
                  (\moduleName ->
                      hsep
                        $ catMaybes
                            [ Just "module"
                            , Just $ pretty moduleName
                            , fmap pretty hsmodExports
                            , Just "where"
                            ])
                  hsmodName
              , case hsmodImports of
                  []  ->
                    Nothing

                  _ ->
                    Just (pretty hsmodImports)
              ]
      , pretty hsmodDecls
      ]


instance Pretty (Located (HsDecl RdrName)) where
  pretty (L _loc decl) =
    pretty decl

  prettyList =
    concatWith
      (\x y ->
          x <> hardline <> hardline <> hardline <> y)
      . map (hardVsep . map pretty)
      . groupDecls


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
  pretty (AnnD ann) =
    pretty ann
  pretty (DerivD d) =
    pretty d


instance Pretty (DerivDecl RdrName) where
  pretty (DerivDecl t _) =
    "deriving instance" <+> pretty t


instance Pretty (AnnDecl RdrName) where
  pretty (HsAnnotation st _ expr) =
    pretty st <+> pretty expr <+> "#-}"


instance Pretty (SpliceDecl RdrName) where
  pretty (SpliceDecl a _) =
    pretty a


instance Pretty (Located (HsSplice RdrName)) where
  pretty (L _loc a) =
    pretty a


instance Pretty (TyClDecl RdrName) where
  pretty SynDecl {tcdLName, tcdRhs} =
    "type" <+> pretty tcdLName <+> equals <> hardline
      <> indent 2 (pretty tcdRhs)
  pretty DataDecl {..} =
    let
      HsDataDefn {..} =
        tcdDataDefn

    in
      nest 2
        $ (case dd_ND of
             NewType  ->
               "newtype"

             DataType  ->
               "data")
        <+> pretty tcdLName
        <> (case hsq_explicit tcdTyVars of
              []  ->
                mempty

              vars ->
                space <> hsep (map pretty vars))
        <> (case dd_cons of
              []  ->
                mempty

              cons ->
                space <> equals <+> pretty dd_cons)
        <> foldMap
             (\a ->
                 space <> "deriving" <+> pretty a)
             dd_derivs
  pretty ClassDecl {..} =
    "class" <+> pretty tcdLName <+> pretty tcdTyVars <+> "where" <> hardline
      <> indent 2
           (concatWith
              (\x y ->
                  x <> hardline <> hardline <> y)
              $ map (prettyBind equals . unLoc) (toList tcdMeths)
              ++ map pretty tcdSigs)


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
  pretty ConDeclH98 {con_name, con_details, con_cxt, con_qvars} =
    (foldMap
       (\vars ->
           "forall" <+> pretty con_qvars <> dot <> space)
       con_qvars)
      <> (foldMap
            (\ctx ->
                pretty ctx <+> "=>" <> space)
            con_cxt)
      <> pretty con_name
      <+> (case con_details of
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
    pretty cd_fld_names <+> "::" <+> align (pretty cd_fld_type)


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
  pretty TyFamInstD {tfid_inst} =
    pretty tfid_inst


instance Pretty (TyFamInstDecl RdrName) where
  pretty =
    pretty . tfid_eqn


instance Pretty (LTyFamInstEqn RdrName) where
  pretty =
    pretty . unLoc


instance Pretty (TyFamInstEqn RdrName) where
  pretty (TyFamEqn n pats rhs) =
    hang 2 $ "type instance" <+> pretty n <+> pretty pats <+> equals <> hardline
      <> pretty rhs


instance Pretty (HsTyPats RdrName) where
  pretty =
    pretty . hsib_body


instance Pretty (ClsInstDecl RdrName) where
  pretty ClsInstDecl {cid_poly_ty, cid_binds} =
    "instance" <+> align (pretty cid_poly_ty) <+> "where" <> hardline
      <> indent 2
           (concatWith
              (\x y ->
                  x <> hardline <> hardline <> y)
              (map (prettyBind equals . unLoc) (toList cid_binds)))


instance Pretty (Located (StmtLR RdrName RdrName (LHsExpr RdrName))) where
  pretty (L _loc a) =
    pretty a


instance Pretty body => Pretty (StmtLR RdrName RdrName body) where
  pretty (BindStmt p bod _ _ _) =
    align $ hang 2 (parPat (unLoc p)) <+> "<-" <> hardline
      <> indent 2 (pretty bod)
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
  pretty (HsIsString st _) =
    pretty st
  pretty (HsFractional ft) =
    pretty (fl_text ft)


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
parensExpr (expr@ExprWithTySig {}) =
  parens (pretty expr)
parensExpr (expr@OpApp {}) =
  parens (pretty expr)
parensExpr a =
  pretty a


parensLeftOp (expr@HsLam {}) =
  parens (pretty expr)
parensLeftOp (expr@HsCase {}) =
  parens (pretty expr)
parensLeftOp (expr@HsIf {}) =
  parens (pretty expr)
parensLeftOp (expr@HsLet {}) =
  parens (pretty expr)
parensLeftOp (expr@HsDo {}) =
  parens (pretty expr)
parensLeftOp (expr@NegApp {}) =
  parens (pretty expr)
parensLeftOp (expr@ExprWithTySig {}) =
  parens (pretty expr)
parensLeftOp a =
  pretty a


instance Pretty (HsExpr RdrName) where
  pretty (HsVar id_) =
    pretty id_
  pretty (HsOverLit a) =
    pretty a
  pretty (HsLit lit) =
    pretty lit
  pretty (HsLam mg) =
    "\\" <> align (prettyMatchGroup "->" mg)
  pretty (HsApp a b) =
    group . hang 2 $ parensExpr (unLoc a) <> line <> parensExpr (unLoc b)
  pretty (OpApp (L _ a) (L _ (HsVar op)) _ (L _ b))
    | HSFmt.isSymOcc op = group $ hang 2 $ parensLeftOp a <> line
                            <> prettyName (unLoc op)
                            <+> pretty b
    | otherwise = group $ hang 2 $ parensLeftOp a <> line <> "`" <> pretty op
                    <> "`"
                    <+> pretty b
  pretty (OpApp a other _ b) =
    error "OpApp with a non-HsVar operator"
  pretty (NegApp a _) =
    "-" <> pretty a
  pretty (HsPar expr) =
    parens (pretty expr)
  pretty (ExplicitTuple args _) =
    parens $ hsep $ punctuate comma (map pretty args)
  pretty (HsCase expr MG {mg_alts}) =
    align $ "case" <+> align (pretty expr) <+> "of" <> hardline
      <> indent 2
           (concatWith
              (\x y ->
                  x <> hardline <> hardline <> y)
              (map (prettyMatch "->" (hang 2) . unLoc) (unLoc mg_alts)))
  pretty (HsIf _ a b c) =
    align $ "if" <+> align (pretty a) <+> "then" <> hardline
      <> indent 2 (pretty b)
      <> hardline
      <> "else"
      <> hardline
      <> indent 2 (pretty c)
  pretty (HsLet binds expr) =
    align $ "let" <> hardline
      <> indent 2 (prettyHsLocalBinds equals (unLoc binds))
      <> hardline
      <> hardline
      <> "in"
      <> hardline
      <> indent 2 (pretty expr)
  pretty (HsDo _ exprs _) =
    align $ "do" <> hardline
      <> indent 2
           (concatWith
              (\x y ->
                  x <> hardline <> hardline <> y)
              (map (hang 2 . pretty) (unLoc exprs)))
  pretty (ExplicitList _ _ exprs) =
    list (map (align . pretty) exprs)
  pretty RecordCon {rcon_con_name, rcon_flds} =
    group (hang 2 (pretty rcon_con_name <> line <> pretty rcon_flds))
  pretty (HsSpliceE a) =
    pretty a
  pretty (ExprWithTySig (L _ a) b) =
    parens (parensExpr a <+> "::" <+> pretty b)
  pretty RecordUpd {rupd_expr, rupd_flds} =
    group (hang 2 (parensExpr (unLoc rupd_expr) <> line <> pretty rupd_flds))
  pretty (HsProc pat cmds) =
    align $ "proc" <+> align (pretty pat) <+> "->" <+> pretty cmds
  pretty (SectionR (L _ (HsVar op)) a) =
    prettyName (unLoc op) <+> pretty a
  pretty (SectionL a (L _ (HsVar op))) =
    pretty a <+> prettyName (unLoc op)
  pretty (ArithSeq _ _ arithSeq) =
    lbracket <> pretty arithSeq <> rbracket


instance Pretty (ArithSeqInfo RdrName) where
  pretty (From a) =
    pretty a <> ".."
  pretty (FromThen a b) =
    pretty a <> comma <+> pretty b <> comma <+> ".."
  pretty (FromTo a b) =
    pretty a <+> ".." <+> pretty b
  pretty (FromThenTo a b c) =
    pretty a <> comma <+> pretty b <> comma <+> ".." <+> pretty c


instance Pretty (Located (HsCmdTop RdrName)) where
  pretty (L _ a) =
    pretty a


instance Pretty (HsCmdTop RdrName) where
  pretty (HsCmdTop cmd _ _ _) =
    pretty cmd


instance Pretty (Located (HsCmd RdrName)) where
  pretty (L _ a) =
    pretty a


instance Pretty (HsCmd RdrName) where
  pretty (HsCmdDo stmts _) =
    "do" <+> align (pretty stmts)
  pretty (HsCmdArrApp expr args _ HsFirstOrderApp  True ) =
    pretty expr <+> "-<" <+> pretty args


instance Pretty (Located [CmdLStmt RdrName]) where
  pretty (L _ a) =
    align
      $ concatWith
          (\x y ->
              x <> hardline <> hardline <> y)
          (map pretty a)


instance Pretty (Located (StmtLR RdrName RdrName (LHsCmd RdrName))) where
  pretty (L _ a) =
    pretty a


instance Pretty (Located (HsRecUpdField RdrName)) where
  pretty (L _loc a) =
    pretty a

  prettyList xs =
    lbrace <+> hsep (punctuate comma (map pretty xs)) <+> rbrace


instance Pretty (HsRecUpdField RdrName) where
  pretty HsRecField {hsRecFieldLbl, hsRecFieldArg} =
    pretty hsRecFieldLbl <+> equals <+> pretty hsRecFieldArg


instance Pretty (Located (AmbiguousFieldOcc RdrName)) where
  pretty (L _loc a) =
    pretty a


instance Pretty (AmbiguousFieldOcc RdrName) where
  pretty (Unambiguous n _) =
    pretty n
  pretty (Ambiguous n _) =
    pretty n


instance Pretty (HsRecordBinds RdrName) where
  pretty HsRecFields {rec_flds, rec_dotdot} =
    braces
      (hsep . punctuate comma $ map pretty rec_flds
         ++ (case rec_dotdot of
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
    brackets
      (prettyInfixName b <> "|"
         <> column
              (\n ->
                  indent (negate n) (pretty src))
         <> "|")
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
  pretty (HsChar src _) =
    pretty src


parPat (a@(ConPatIn _ InfixCon {})) =
  parens (pretty a)
parPat (a@AsPat {}) =
  parens (pretty a)
parPat (a@ViewPat {}) =
  parens (pretty a)
parPat a =
  pretty a


instance Pretty (Pat RdrName) where
  pretty WildPat {} =
    "_"
  pretty (VarPat name) =
    pretty name
  pretty (AsPat id_ pat) =
    pretty id_ <> "@" <> parPat (unLoc pat)
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
  pretty (ViewPat expr pat _) =
    align (parensExpr (unLoc expr)) <+> "->" <+> pretty pat
  pretty (NPat l neg _ _) =
    foldMap (const "-") neg <> pretty l


instance Pretty (Located (HsOverLit RdrName)) where
  pretty (L _ a) =
    pretty a


instance Pretty (HsConPatDetails RdrName) where
  pretty (PrefixCon args) =
    hsep (map pretty args)
  pretty (RecCon rec_) =
    pretty rec_
  pretty (InfixCon a b) =
    parens $ pretty a <+> pretty b


instance Pretty (HsRecFields RdrName (LPat RdrName)) where
  pretty HsRecFields {rec_flds, rec_dotdot} =
    braces $ hsep $ punctuate comma $ map pretty rec_flds
      ++ maybe [] (const [".."]) rec_dotdot


instance Pretty (Located (HsRecField RdrName (LPat RdrName))) where
  pretty (L _loc a) =
    pretty a


instance Pretty (HsRecField RdrName (LPat RdrName)) where
  pretty HsRecField {..} =
    pretty hsRecFieldLbl
      <> (if hsRecPun then
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
    hsep (punctuate comma (map pretty names)) <+> "::" <+> align (pretty sig)
  pretty (ClassOpSig isDefault b c) =
    (if isDefault then
       "default" <> space
     else
       mempty)
      <> hsep (punctuate comma (map pretty b))
      <+> "::"
      <+> align (pretty c)


instance Pretty (LHsSigWcType RdrName) where
  pretty HsIB {hsib_body} =
    pretty hsib_body


instance Pretty (LHsWcType RdrName) where
  pretty HsWC {hswc_body} =
    pretty hswc_body


instance Pretty (LHsType RdrName) where
  pretty (L _loTcT ty) =
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
    pretty a <+> pretty b
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
  pretty (HsTyLit t) =
    pretty t
  pretty (HsExplicitTupleTy _ tys) =
    squote <> tupled (map pretty tys)
  pretty (HsBangTy _ t) =
    "!" <> pretty t


instance Pretty HsTyLit where
  pretty (HsNumTy src _) =
    pretty src
  pretty (HsStrTy src _) =
    pretty src


instance Pretty (Located (HsAppType RdrName)) where
  pretty (L _loc appty) =
    pretty appty

  prettyList =
    hsep . map pretty


instance Pretty (HsAppType RdrName) where
  pretty (HsAppPrefix t) =
    pretty t
  pretty (HsAppInfix t) =
    prettyName (unLoc t)


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
    hsep
      $ catMaybes
          [ Just "import"
          , if ideclSource then
              Just "{-# SOURCE #-}"
            else
              Nothing
          , if ideclQualified then
              Just "qualified"
            else
              Nothing
          , Just (pretty ideclName)
          , fmap
              (\as ->
                  "as" <+> pretty as)
              ideclAs
          , fmap
              (\(hiding, things) ->
                  hsep
                    $ catMaybes
                        [ if hiding then
                            Just "hiding"
                          else
                            Nothing
                        , Just (align (pretty things))
                        ])
              ideclHiding
          ]


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
  pretty (IEThingWith name _ names _) =
    pretty name <> tupled (map pretty names)
  pretty (IEModuleContents mod) =
    "module" <+> pretty mod


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

    Orig _mod name ->
      pretty name

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
  concatWith
    (\x y ->
        x <> hardline <> y)


prettyBind :: Doc ann -> HsBind RdrName -> Doc ann
prettyBind bind hsBind =
  case hsBind of
    FunBind {fun_id, fun_matches} ->
      hardVsep
        $ map
            (\alt ->
                pretty fun_id <+> prettyMatch bind align (unLoc alt))
            (unLoc $ mg_alts fun_matches)

    PatBind {pat_lhs, pat_rhs} ->
      hang 2 (parPat (unLoc pat_lhs)) <+> prettyGRHSs bind pat_rhs

    VarBind {var_id, var_rhs} ->
      prettyInfixName var_id <+> bind <> hardline <> indent 2 (pretty var_rhs)


prettyMatch :: Doc ann -> (Doc ann -> Doc ann) -> Match RdrName (LHsExpr RdrName) -> Doc ann
prettyMatch bind alignPatterns Match {m_pats, m_grhss} =
  (case m_pats of
     []  ->
       mempty

     _ ->
       alignPatterns (hsep (map (parPat . unLoc) m_pats) <> space))
    <> prettyGRHSs bind m_grhss


prettyGRHSs bind GRHSs {grhssGRHSs, grhssLocalBinds} =
  (case grhssGRHSs of
     [grhs] ->
       prettyGRHS bind (unLoc grhs)

     _ ->
       hardline <> indent 2 (vsep (map (prettyGRHS bind . unLoc) grhssGRHSs)))
    <> (case unLoc grhssLocalBinds of
          EmptyLocalBinds  ->
            mempty

          _ ->
            hardline <> hardline
              <> indent 2
                   ("where" <> hardline <> hardline
                      <> indent 2
                           (prettyHsLocalBinds bind (unLoc grhssLocalBinds))))


prettyGRHS bind (GRHS []  body) =
  bind <> hardline <> indent 2 (pretty body)
prettyGRHS bind (GRHS guards body) =
  "|"
    <+> align
          (hsep (punctuate comma (map (prettyGuard . unLoc) guards)) <+> bind
             <+> pretty body)


prettyGuard :: GuardStmt RdrName -> Doc ann
prettyGuard (a@(BodyStmt (L _ e) _ _ _)) | needsParens e = parens (pretty a)

  where

    needsParens (ExprWithTySig {}) =
      True
    needsParens HsLam {} =
      True
    needsParens (HsIf _ _ _ (L _ (ExprWithTySig {}))) =
      True
    needsParens HsCase {} =
      True
    needsParens _ =
      False
prettyGuard a =
  pretty a


prettyHsLocalBinds bind (HsValBinds b) =
  prettyHsValBindsLR bind b


prettyHsValBindsLR bind (ValBindsIn bnds _) =
  concatWith
    (\x y ->
        x <> hardline <> hardline <> y)
    $ map (prettyBind bind . unLoc) (toList bnds)


prettyMatchGroup bind MG {mg_alts} =
  hardVsep $ map (prettyMatch bind align . unLoc) (unLoc mg_alts)


atLeastAlign d =
  column
    (\k ->
        nesting
          (\i ->
              nest (max 0 (k - i)) d))


tupled :: [Doc ann] -> Doc ann
tupled =
  group
    . encloseSepAligning atLeastAlign (lparen <> flatAlt space mempty)
        (line' <> rparen)
        (comma <> space)


list :: [Doc ann] -> Doc ann
list =
  group
    . encloseSepAligning atLeastAlign (lbracket <> flatAlt space mempty)
        (line' <> rbracket)
        (comma <> space)


encloseSepAligning :: (Doc ann -> Doc ann) -> Doc ann -> Doc ann -> Doc ann -> [Doc ann] -> Doc ann
encloseSepAligning align' l r s ds =
  case ds of
    []  ->
      l <> r

    [d] ->
      l <> d <> r

    _ ->
      align' (cat (zipWith (<>) (l : repeat s) ds) <> r)

module Main where

import qualified Data.Text.Lazy.IO as T
import Data.Text.Lazy (pack, singleton)
import Data.Foldable (toList)
import Data.Data
import Prelude hiding ((<$>))
import GHC hiding (parseModule)
import qualified Name as GHC
import qualified Module as GHC
import qualified OccName as GHC
import Language.Haskell.GHC.ExactPrint (parseModule)
import FastString
import Text.PrettyPrint.Leijen.Text.Monadic hiding (tupled)
import RdrName
import Control.Monad.Trans.Reader (Reader, ask, runReader, withReader)

main :: IO ()
main = prettyPrintFile "HSFmt.hs" >>= T.putStrLn . displayT . renderPretty 1 80
prettyPrintFile :: FilePath -> IO (Doc)
prettyPrintFile path = do
  out <-
    parseModule path
  case out of
    Left e -> error (show e)
    Right (_, parsed) -> return $ runReader (pp parsed) initialPrintState
data  PrintState = PrintState { bindSymbol :: Reader PrintState Doc }
initialPrintState :: PrintState
initialPrintState = PrintState {bindSymbol = string "="}
class Print a where
  pp :: a -> Reader PrintState Doc
instance (Print e) => Print (GenLocated l e) where
  pp (L _ a) = pp a
instance (IsSymOcc a, Print a) => Print (HsModule a) where
  pp HsModule {..} = (case hsmodName of
                        Just name -> hang 2
                                       (string "module" </> pp name
                                       <>
                                       (case hsmodExports of
                                          Nothing -> empty
                                          Just (L _ []) -> softline
                                            <>
                                            string "()"
                                          Just (L _ (e : es)) -> line
                                            <>
                                            vsep
                                              (do
                                              x <-
                                                string "(" <> space <> pp e
                                              xs <-
                                                mapM (\a -> string ", " <> pp a)
                                                  es
                                              pure (x : xs))
                                            <>
                                            line
                                            <>
                                            string ")")
                                       </>
                                       string "where")
                          <>
                          line
                          <>
                          line)
    <>
    (vsep (mapM pp hsmodImports))
    <>
    line
    <>
    line
    <>
    (vsep (mapM pp hsmodDecls))
instance (Print name) => Print (HsDecl name) where
  pp (SigD sig) = pp sig
  pp (ValD bind) = pp bind
  pp (InstD a) = pp a
  pp (TyClD a) = pp a
instance (Print name) => Print (InstDecl name) where
  pp ClsInstD {..} = pp cid_inst
instance (Print name) => Print (ClsInstDecl name) where
  pp ClsInstDecl {..} = string "instance" <+> pp cid_poly_ty <+> string "where"
    <$>
    indent 2 (vsep (mapM pp (toList cid_binds)))
instance (Print name) => Print (TyClDecl name) where
  pp ClassDecl {..} = string "class" <+> pp tcdLName <+> pp tcdTyVars
    <+>
    string "where"
    <$>
    indent 2 (vsep (mapM pp tcdSigs))
  pp DataDecl {..} = let HsDataDefn {..} = tcdDataDefn
                     in (case dd_ND of
                           NewType -> string "newtype"
                           DataType -> string "data")
                        <+>
                        pp (fmap CommaList dd_ctxt)
                        <+>
                        pp tcdLName
                        <+>
                        pp tcdTyVars
                        <+>
                        equals
                        <+>
                        pp (ConstructorList dd_cons)
instance (Print name) => Print (LHsQTyVars name) where
  pp HsQTvs {..} = hsep (mapM pp hsq_explicit)
instance (Print name) => Print (HsTyVarBndr name) where
  pp (UserTyVar n) = pp n
newtype  ConstructorList thing = ConstructorList [thing]
instance (Print thing) => Print (ConstructorList thing) where
  pp (ConstructorList as) = cat (punctuate (string "|") (mapM pp as))
instance (Print name) => Print (ConDecl name) where
  pp ConDeclH98 {..} = pp con_name
    <+>
    case con_details of
      RecCon xs -> lbrace <+> pp (CommaList (unLoc xs)) <+> rbrace
      PrefixCon args -> hsep (mapM pp args)
instance (Print idL, Print idR) => Print (HsBindLR idL idR) where
  pp FunBind {..} = vsep
                      (mapM (pp . SingleMatch fun_id)
                         (unLoc (mg_alts fun_matches)))
  pp PatBind {..} = pp pat_lhs <+> pp pat_rhs
  pp VarBind {..} = error "VarBind"
instance (Print name) => Print (ConDeclField name) where
  pp ConDeclField {..} = pp (CommaList cd_fld_names) <+> string "::"
    <+>
    pp cd_fld_type
instance (Print id, Print body) => Print (MatchGroup id body) where
  pp MG {..} = vsep (mapM pp (unLoc mg_alts))
data  SingleMatch name id body = SingleMatch { singleMatchName :: name, 
                                               singleMatchAlt :: LMatch id body }
instance (Print name, 
          Print id, 
          Print body) => Print (SingleMatch name id body) where
  pp SingleMatch {..} = pp singleMatchName <+> pp singleMatchAlt
instance (Print id, Print body) => Print (Match id body) where
  pp Match {..} = cat (mapM (\pat -> pp pat <> space) m_pats)
    <>
    (case m_type of
       Nothing -> empty
       Just a -> pp a)
    <>
    nest 2 (pp m_grhss)
instance (Print body, Print id) => Print (GRHSs id body) where
  pp GRHSs {..} = vsep (mapM pp grhssGRHSs)
instance (Print body, Print id) => Print (GRHS id body) where
  pp (GRHS [] body) = do
    PrintState{bindSymbol} <-
      ask
    bindSymbol <+> pp body
  pp (GRHS guards body) = do
    PrintState{bindSymbol} <-
      ask
    hang 2
      (string "|" <+> pp (CommaList guards) <+> bindSymbol
      <+>
      indent 2 (pp body))
newtype  CommaList a = CommaList [a]
instance (Print name) => Print (CommaList name) where
  pp (CommaList names) = hang 0
    $
    cat (punctuate (comma <> space) (mapM pp names))
instance (Print name) => Print (Sig name) where
  pp (TypeSig names sig) = hang 2 (pp (SimpleTypeSig names sig))
  pp (ClassOpSig isDefault names sig) = hang 2
    $
    (if isDefault then string "default" <> space else empty)
    <>
    pp (SimpleTypeSig names sig)
data  SimpleTypeSig name ty = SimpleTypeSig [name] ty
instance (Print name, Print ty) => Print (SimpleTypeSig name ty) where
  pp (SimpleTypeSig names ty) = group $ pp (CommaList names) <$> string "::"
    <+>
    group (pp ty)
instance (Print name, Print thing) => Print (HsWildCardBndrs name thing) where
  pp (HsWC _placeholder _ ty) = pp ty
instance (Print name, Print thing) => Print (HsImplicitBndrs name thing) where
  pp (HsIB _ thing) = pp thing
bindRArrow = withReader (\s -> s { bindSymbol = string "->" })
instance (Print name) => Print (HsExpr name) where
  pp (HsVar n) = pp n
  pp HsUnboundVar{} = error "HsUnboundVar"
  pp HsRecFld{} = error "HsRecFld"
  pp HsOverLabel{} = error "HsOverLabel"
  pp HsIPVar{} = error "HsIPVar"
  pp (HsOverLit lit) = pp lit
  pp (HsLit lit) = pp lit
  pp (HsLam mg) = string "\\" <> bindRArrow (pp mg)
  pp HsLamCase{} = error "HsLamCase"
  pp (HsApp l r) = group $ hang 2 (pp l <$> group (pp r))
  pp HsAppType{} = error "HsAppType"
  pp HsAppTypeOut{} = error "HsAppTypeOut"
  pp (OpApp a op _ b) = group (pp a <$> pp op <$> pp b)
  pp (NegApp neg _) = string "-" <> pp neg
  pp (HsPar expr) = lparen <> pp expr <> rparen
  pp (SectionL a b) = lparen <> pp a <+> pp b <> rparen
  pp SectionR{} = error "HsSectionR"
  pp (HsDo _ctx statements _) = string "do"
    <$>
    vsep (mapM pp (unLoc statements))
  pp (HsCase expr patterns) = hang 2
                                (string "case" <+> pp expr <+> string "of"
                                <$>
                                (bindRArrow (pp patterns)))
  pp (HsIf _ cond a b) = string "if" <+> pp cond <+> string "then" <+> pp a
    <+>
    string "else"
    <+>
    pp b
  pp (HsLet binds expr) = align $ string "let" <+> align (pp binds)
    <$>
    string "in"
    <+>
    align (pp expr)
  pp (ExplicitList _ _ elems) = lbracket <> pp (CommaList elems) <> rbracket
  pp RecordCon {..} = pp rcon_con_name <+> lbrace <> pp rcon_flds <> rbrace
  pp RecordUpd {..} = pp rupd_expr <+> lbrace <+> pp (CommaList rupd_flds)
    <+>
    rbrace
instance (Print id, Print arg) => Print (HsRecFields id arg) where
  pp (HsRecFields [] Nothing) = empty
  pp (HsRecFields [] (Just 0)) = string ".."
  pp (HsRecFields flds Nothing) = pp (CommaList flds)
instance (Print idL, Print idR) => Print (HsLocalBindsLR idL idR) where
  pp (HsValBinds binds) = pp binds
instance (Print idL, Print idR) => Print (HsValBindsLR idL idR) where
  pp (ValBindsIn binds _) = vsep (mapM pp (toList binds))
instance (Print name) => Print (AmbiguousFieldOcc name) where
  pp (Unambiguous n _) = pp n
  pp (Ambiguous n _) = pp n
instance (Print name) => Print (HsOverLit name) where
  pp OverLit {..} = pp ol_val
instance Print OverLitVal where
  pp (HsIsString _ str) = pp str
  pp (HsIntegral _ n) = pp n
instance Print Integer where
  pp = integer
instance Print HsLit where
  pp (HsString _ str) = pp str
  pp (HsChar _ c) = squote <> string (singleton c) <> squote
instance Print FastString where
  pp fs = string (pack (show (unpackFS fs)))
instance (Print body, Print idL) => Print (StmtLR idL idR body) where
  pp (BodyStmt body _syntax1 _syntax2 _placeholder) = pp body
  pp (BindStmt pat body _syn1 _syn2 _placeholder) = hang 2
                                                      (pp pat <+> string "<-"
                                                      <$>
                                                      pp body)
instance (Print name) => Print (Pat name) where
  pp WildPat{} = string "_"
  pp (VarPat name) = pp name
  pp LazyPat{} = error "LazyPat"
  pp (AsPat x y) = pp x <> string "@" <> pp y
  pp (ParPat p) = lparen <> pp p <> rparen
  pp BangPat{} = error "BangPat"
  pp (ListPat pats _ _) = lbracket <> pp (CommaList pats) <> rbracket
  pp (TuplePat elems _ _) = tupled elems
  pp PArrPat{} = error "ParrPat"
  pp (ConPatIn name (PrefixCon args)) = pp name <+> hsep (mapM pp args)
  pp (ConPatIn name (InfixCon l r)) = pp l <+> pp name <+> pp r
  pp (ConPatIn name (RecCon hsRecFields@(HsRecFields [] (Just 0)))) = pp name
    <+>
    lbrace
    <>
    pp hsRecFields
    <>
    rbrace
  pp (ConPatIn name (RecCon hsRecFields)) = pp name <> lbrace <> pp hsRecFields
    <>
    rbrace
  pp ConPatOut {..} = error "ConPatOut"
  pp ViewPat{} = error "ViewPat"
  pp SplicePat{} = error "SplicePat"
  pp LitPat{} = error "LitPat"
  pp (NPat n _ _ _) = pp n
  pp NPlusKPat{} = error "NPlusKPat"
  pp SigPatIn{} = error "SigPatIn"
  pp SigPatOut{} = error "SigPatOut"
  pp CoPat{} = error "CoPat"
instance (Print id, Print arg) => Print (HsRecField' id arg) where
  pp HsRecField {..} = pp hsRecFieldLbl
    <>
    (if hsRecPun then empty else space <> equals <+> pp hsRecFieldArg)
instance (Print name) => Print (FieldOcc name) where
  pp FieldOcc {..} = pp rdrNameFieldOcc
instance (Print name) => Print (HsType name) where
  pp HsForAllTy{} = error "HsForAllTy"
  pp HsQualTy {..} = (case unLoc hst_ctxt of
                        [] -> empty
                        ctx -> case ctx of
                                 [one] -> pp one
                                 many -> lparen <> pp (CommaList many) <> rparen
                          <+>
                          string "=>"
                          <>
                          space)
    <>
    pp hst_body
  pp (HsTyVar (L _ name)) = pp name
  pp (HsAppsTy types) = hsep (mapM pp types)
  pp (HsAppTy l r) = pp l <+> pp r
  pp (HsFunTy (L _ a) (L _ b)) = pp a <$> string "->" <+> pp b
  pp HsPArrTy{} = error "HsPArrTy"
  pp (HsTupleTy _ ts) = tupled ts
  pp (HsParTy (L _ name)) = lparen <> pp name <> rparen
  pp (HsListTy (L _ name)) = lbracket <> pp name <> rbracket
instance (Print name) => Print (HsAppType name) where
  pp (HsAppPrefix (L _ a)) = pp a
instance (Print name, IsSymOcc name) => Print (ImportDecl name) where
  pp ImportDecl {..} = string "import"
    <+>
    (if ideclQualified then string "qualified" <> space else empty)
    <>
    pp ideclName
    <>
    (case ideclAs of
       Just n -> space <> string "as" <+> pp n
       Nothing -> empty)
    <>
    (case ideclHiding of
       Just (hiding, L _ names) -> space
         <>
         (if hiding then string "hiding" <> space else empty)
         <>
         tupled names
       Nothing -> empty)
instance (IsSymOcc a, Print a) => Print (IE a) where
  pp (IEVar n) = pp (fmap InfixOccName n)
  pp (IEThingAbs n) = pp (fmap InfixOccName n)
instance Print RdrName where
  pp (Unqual occName) = pp occName
  pp (Qual mod name) = pp mod <> dot <> pp name
  pp (Orig mod name) = pp mod <> dot <> pp name
  pp (Exact name) = pp (GHC.nameOccName name)
instance Print Module where
  pp = pp . moduleName
newtype  InfixOccName name = InfixOccName name
class IsSymOcc a where
  isSymOcc :: a -> Bool
instance IsSymOcc GHC.OccName where
  isSymOcc = GHC.isSymOcc
instance IsSymOcc RdrName where
  isSymOcc = isSymOcc . rdrNameOcc
instance (Print name, IsSymOcc name) => Print (InfixOccName name) where
  pp (InfixOccName occName) | isSymOcc occName =   lparen <> pp occName
                                                   <>
                                                   rparen
    | otherwise =   pp occName
instance Print GHC.OccName where
  pp n = string (pack (GHC.occNameString n))
instance Print ModuleName where
  pp = string . pack . GHC.moduleNameString
tupled :: (Print a) => [a] -> Reader PrintState Doc
tupled xs = lparen <> pp (CommaList xs) <> rparen

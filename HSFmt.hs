module Main where

import BasicTypes (FractionalLit(..), SourceText)
import Control.Applicative (liftA2)
import Control.Monad (guard, zipWithM)
import Control.Monad.Reader.Class (ask, local)
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.Writer (WriterT, runWriterT, listen)
import Control.Monad.Writer.Class (tell)
import Data.Data
import Data.Foldable (toList)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Monoid (Any(..))
import Data.Text.Lazy (pack, singleton)
import qualified Data.Text.Lazy.IO as T
import Data.Traversable (for)
import FastString
import GHC hiding (parseModule)
import Language.Haskell.GHC.ExactPrint
         (Annotation(..), Anns, AnnKey(..), parseModule)
import Language.Haskell.GHC.ExactPrint.Types (annGetConstr, commentContents)
import qualified Module as GHC
import qualified Name as GHC
import NameSet (NameSet)
import qualified OccName as GHC
import Prelude hiding ((<$>), lines)
import RdrName
import System.Environment
import qualified Text.PrettyPrint.Leijen.Text as PP
import qualified Text.PrettyPrint.Leijen.Text.Monadic as PPM
import Text.PrettyPrint.Leijen.Text.Monadic hiding (group, tupled)

type PrintM a = WriterT Any (Reader PrintState) a

-- Group 'LHsDecl's into logical groups. For example, we combine type signatures
-- with all bindings for that type signature.
groupDecls :: Eq id => [LHsDecl id] -> [[LHsDecl id]]
groupDecls [] = []
groupDecls (x@(L _ (SigD (TypeSig names _))) : xs) =
  let (binds, rest) = splitNames (fmap unLoc names) xs
  in (x : binds) : groupDecls rest
groupDecls (x : xs) = [x] : groupDecls xs

splitNames :: Eq id => [id] -> [LHsDecl id] -> ([LHsDecl id], [LHsDecl id])
splitNames names [] = ([], [])
splitNames names (x : xs) =
  case unLoc x of
    ValD (FunBind {..}) ->
      if unLoc fun_id `elem` names
        then let (binds, rest) = splitNames names xs
             in (x : binds, rest)
        else ([], x : xs)
    _ -> ([], x : xs)

main :: IO ()
main =
  do [file] <- getArgs
     prettyPrintFile file >>= T.putStrLn . displayT . renderPretty 1 80

prettyPrintFile :: FilePath -> IO (Doc)
prettyPrintFile path =
  do out <- parseModule path
     case out of
       Left e -> error (show e)
       Right (anns, parsed) ->
         return $
         runReader (fmap fst (runWriterT (pp parsed))) (initialPrintState anns)

data PrintState = PrintState { bindSymbol :: PrintM Doc, anns :: Anns }

initialPrintState :: Anns -> PrintState
initialPrintState anns = PrintState {bindSymbol = string "=", anns = anns}

class Print a where
  pp :: a -> PrintM Doc

instance (Print e) => Print (GenLocated l e) where
  pp (L _ a) = pp a

instance (IsSymOcc a, Print a, Eq a, Data a, DataId a)
           => Print (HsModule a) where
  pp HsModule {..} =
    let moduleName =
          flip fmap hsmodName $
          \name ->
            group $
            hang 2
              (string "module" </> pp name <>
               (case hsmodExports of
                  Nothing -> empty
                  Just (L _ []) -> line <> string "()"
                  Just (L _ es) -> line <> expandedTupled es) </>
               string "where")
        imports =
          do guard (not (null hsmodImports))
             return (lines (mapM pp hsmodImports))
        decls =
          do guard (not (null hsmodDecls))
             return
               (paragraphs
                  (mapM (lines . mapM (pp . CommentedDecl))
                     (groupDecls hsmodDecls)))
    in paragraphs (sequence (catMaybes [moduleName, imports, decls]))

newtype CommentedDecl id = CommentedDecl (LHsDecl id)

instance (Print id, IsSymOcc id, Data id, DataId id)
           => Print (CommentedDecl id) where
  pp (CommentedDecl (L l a@(SigD typeSig@TypeSig{}))) =
    do PrintState{anns} <- ask
       (case Map.lookup (AnnKey l (annGetConstr typeSig)) anns of
          Just Ann {..} ->
            case annPriorComments of
              [] -> empty
              comments ->
                lines
                  (mapM (\(comment, _) -> text (pack (commentContents comment)))
                     comments) <>
                mandatoryLine
          Nothing -> empty) <>
         pp a
  pp (CommentedDecl other) = pp other

instance (Print name, IsSymOcc name) => Print (HsDecl name) where
  pp (SigD sig) = pp sig
  pp (ValD bind) = pp bind
  pp (InstD a) = pp a
  pp (TyClD a) = pp a

instance (Print name, IsSymOcc name) => Print (InstDecl name) where
  pp ClsInstD {..} = pp cid_inst

instance (Print name, IsSymOcc name) => Print (ClsInstDecl name) where
  pp ClsInstDecl {..} =
    string "instance" <+> hang 2 (pp cid_poly_ty) <+> string "where" <$>
    indent 2 (vsep (mapM pp (toList cid_binds)))

instance (Print name, IsSymOcc name) => Print (TyClDecl name) where
  pp SynDecl {..} =
    string "type" <+> pp tcdLName <+> pp tcdTyVars <+> equals <+> pp tcdRhs
  pp ClassDecl {..} =
    string "class" <+> pp tcdLName <+> pp tcdTyVars <+> string "where" <$>
    indent 2 (vsep (mapM pp tcdSigs))
  pp DataDecl {..} =
    let HsDataDefn {..} = tcdDataDefn
    in group $ nest 2 $
       (case dd_ND of
          NewType -> string "newtype"
          DataType -> string "data") <+>
       pp tcdLName <+>
       pp tcdTyVars <+>
       equals <$>
       pp (ConstructorList dd_cons)

instance (Print name) => Print (LHsQTyVars name) where
  pp HsQTvs {..} = hsep (mapM pp hsq_explicit)

instance (Print name) => Print (HsTyVarBndr name) where
  pp (UserTyVar n) = pp n

newtype ConstructorList thing = ConstructorList [thing]

instance (Print thing) => Print (ConstructorList thing) where
  pp (ConstructorList as) = cat (punctuate (string "|") (mapM pp as))

instance (Print name, IsSymOcc name) => Print (ConDecl name) where
  pp ConDeclH98 {..} =
    pp con_name <+>
    case con_details of
      RecCon xs -> align (lbrace <+> pp (CommaList (unLoc xs)) <+> rbrace)
      PrefixCon args -> hsep (mapM pp args)

instance (Print idL, Print idR, IsSymOcc idL, IsSymOcc idR)
           => Print (HsBindLR idL idR) where
  pp FunBind {..} =
    lines (mapM (pp . SingleMatch fun_id) (unLoc (mg_alts fun_matches)))
  pp PatBind {..} = pp pat_lhs <+> pp pat_rhs
  pp VarBind {..} = error "VarBind"

instance (Print name, IsSymOcc name) => Print (ConDeclField name) where
  pp ConDeclField {..} =
    pp (CommaList cd_fld_names) <+> string "::" <+> pp cd_fld_type

instance (Print id, Print body, IsSymOcc id) => Print (MatchGroup id body) where
  pp MG {..} = vsep (mapM pp (unLoc mg_alts))

data SingleMatch name id body =
  SingleMatch { singleMatchName :: name, singleMatchAlt :: LMatch id body }

instance (Print name, Print id, Print body, IsSymOcc name, IsSymOcc id)
           => Print (SingleMatch name id body) where
  pp SingleMatch {..} = pp (InfixOccName singleMatchName) <+> pp singleMatchAlt

instance (Print id, Print body, IsSymOcc id) => Print (Match id body) where
  pp Match {..} =
    cat (mapM (\pat -> pp pat <> space) m_pats) <>
    (case m_type of
       Nothing -> empty
       Just a -> pp a) <>
    pp m_grhss

instance (Print body, Print id, IsSymOcc id) => Print (GRHSs id body) where
  pp GRHSs {..} = vsep (mapM pp grhssGRHSs)

instance (Print body, Print id, IsSymOcc id) => Print (GRHS id body) where
  pp (GRHS [] body) =
    do PrintState{bindSymbol} <- ask
       group (nest 2 (bindSymbol <$> pp body))
  pp (GRHS guards body) =
    do PrintState{bindSymbol} <- ask
       indent 2
         (string "|" <+> pp (CommaList guards) <+> bindSymbol <+>
          indent 2 (pp body))

newtype CommaList a = CommaList [a]

instance (Print name) => Print (CommaList name) where
  pp (CommaList names) =
    hang 0 $ cat (punctuate (comma <> space) (mapM pp names))

instance (Print name, IsSymOcc name) => Print (Sig name) where
  pp (TypeSig names sig) = hang 2 (pp (SimpleTypeSig names sig))
  pp (ClassOpSig isDefault names sig) =
    hang 2 $ (if isDefault then string "default" <> space else empty) <>
    pp (SimpleTypeSig names sig)

data SimpleTypeSig name ty = SimpleTypeSig [name] ty

instance (Print name, Print ty, IsSymOcc name)
           => Print (SimpleTypeSig name ty) where
  pp (SimpleTypeSig names ty) =
    group $ pp (CommaList (fmap InfixOccName names)) <$> string "::" <+>
    group (pp ty)

instance (Print name, Print thing) => Print (HsWildCardBndrs name thing) where
  pp (HsWC _placeholder _ ty) = pp ty

instance (Print name, Print thing) => Print (HsImplicitBndrs name thing) where
  pp (HsIB _ thing) = pp thing

bindRArrow = local (\s -> s { bindSymbol = string "->" })

bindEquals = local (\s -> s { bindSymbol = string "=" })

instance (Print name, IsSymOcc name) => Print (HsExpr name) where
  pp (HsVar n) = pp (InfixOccName n)
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
  pp (OpApp a (L _ (HsVar op)) _ b)   | isSymOcc op =   group
                                                          (pp a <+> pp op <$>
                                                           pp b)
    | otherwise =   pp a <+> char '`' <> pp op <> char '`' <+> pp b
  pp (OpApp a other _ b) = error "OpApp with a non-HsVar operator"
  pp (NegApp neg _) = string "-" <> pp neg
  pp (HsPar expr) = lparen <> nest 1 (pp expr) <> rparen
  pp (SectionL a b) = lparen <> pp a <+> pp b <> rparen
  pp (SectionR a b) = lparen <> pp a <+> pp b <> rparen
  pp (ExplicitTuple exprs _) = tupled exprs
  pp (HsDo _ctx statements _) =
    string "do" <+> align (lines (mapM pp (unLoc statements)))
  pp (HsCase expr patterns) =
    do hang 2
         (string "case" <+> pp expr <+> string "of" <$!>
          bindRArrow (pp patterns))
  pp (HsIf _ cond a b) =
    hang 2
      (string "if" <+> pp cond <$> string "then" <+> pp a <$> string "else" <+>
       pp b)
  pp (HsLet binds expr) =
    align $ string "let" <+> bindEquals (align (pp binds)) <$> string "in" <+>
    align (pp expr)
  pp (ExplicitList _ _ elems) = lbracket <> pp (CommaList elems) <> rbracket
  pp RecordCon {..} = pp rcon_con_name <+> lbrace <> pp rcon_flds <> rbrace
  pp RecordUpd {..} =
    pp rupd_expr <+> lbrace <+> pp (CommaList rupd_flds) <+> rbrace
  pp (ExprWithTySig expr ty) = pp expr <+> string "::" <+> pp ty

instance (Print name, IsSymOcc name) => Print (HsTupArg name) where
  pp (Present expr) = pp expr

instance (Print id, Print arg) => Print (HsRecFields id arg) where
  pp (HsRecFields [] Nothing) = empty
  pp (HsRecFields [] (Just 0)) = string ".."
  pp (HsRecFields flds Nothing) = pp (CommaList flds)

instance (Print idL, Print idR, IsSymOcc idL, IsSymOcc idR)
           => Print (HsLocalBindsLR idL idR) where
  pp (HsValBinds binds) = pp binds

instance (Print idL, Print idR, IsSymOcc idL, IsSymOcc idR)
           => Print (HsValBindsLR idL idR) where
  pp (ValBindsIn binds _) = vsep (mapM pp (toList binds))

instance (Print name) => Print (AmbiguousFieldOcc name) where
  pp (Unambiguous n _) = pp n
  pp (Ambiguous n _) = pp n

instance (Print name) => Print (HsOverLit name) where
  pp OverLit {..} = pp ol_val

instance Print OverLitVal where
  pp (HsIsString _ str) = pp str
  pp (HsIntegral n _) = text (pack n)
  pp (HsFractional n) = pp n

instance Print Integer where
  pp = integer

instance Print FractionalLit where
  pp = text . pack . fl_text

instance Print HsLit where
  pp (HsString _ str) = pp str
  pp (HsChar _ c) = squote <> string (singleton c) <> squote
  pp (HsFloatPrim n) = pp n
  pp (HsDoublePrim n) = pp n

instance Print FastString where
  pp fs = string (pack (show (unpackFS fs)))

instance (Print body, Print idL, Print idR, IsSymOcc idL, IsSymOcc idR)
           => Print (StmtLR idL idR body) where
  pp (BodyStmt body _syntax1 _syntax2 _placeholder) = nest 2 (pp body)
  pp (BindStmt pat body _syn1 _syn2 _placeholder) =
    hang 2 (group (pp pat <+> string "<-" <$> pp body))
  pp (LetStmt binds) = align $ string "let" <+> bindEquals (align (pp binds))

instance (Print name) => Print (Pat name) where
  pp WildPat{} = string "_"
  pp (VarPat name) = pp name
  pp LazyPat{} = error "LazyPat"
  pp (AsPat x y) = pp x <> string "@" <> pp y
  pp (ParPat p) = lparen <> pp p <> rparen
  pp BangPat{} = error "BangPat"
  pp (ListPat pats _ _) = lbracket <> pp (CommaList pats) <> rbracket
  pp (TuplePat elems _ _) = singleLineTuple elems
  pp PArrPat{} = error "ParrPat"
  pp (ConPatIn name (PrefixCon args)) = pp name <+> hsep (mapM pp args)
  pp (ConPatIn name (InfixCon l r)) = pp l <+> pp name <+> pp r
  pp (ConPatIn name (RecCon hsRecFields@(HsRecFields [] (Just 0)))) =
    pp name <+> lbrace <> pp hsRecFields <> rbrace
  pp (ConPatIn name (RecCon hsRecFields)) =
    pp name <> lbrace <> pp hsRecFields <> rbrace
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
  pp HsRecField {..} =
    pp hsRecFieldLbl <>
    (if hsRecPun
       then empty
       else space <> equals <+> pp hsRecFieldArg)

instance (Print name) => Print (FieldOcc name) where
  pp FieldOcc {..} = pp rdrNameFieldOcc

instance (Print name, IsSymOcc name) => Print (HsType name) where
  pp HsForAllTy{} = error "HsForAllTy"
  pp HsQualTy {..} =
    (case unLoc hst_ctxt of
       [] -> empty
       ctx ->
         group $
         (case ctx of
            [one] -> pp one
            ts -> tupled ts) <$>
         string "=>" <>
         space) <>
    pp hst_body
  pp (HsTyVar name) = pp name
  pp (HsAppsTy types) = hsep (mapM pp types)
  pp (HsAppTy l r) = pp l <+> pp r
  pp (HsFunTy a b) = pp a <$> string "->" <+> pp b
  pp HsPArrTy{} = error "HsPArrTy"
  pp (HsTupleTy _ ts) = tupled ts
  pp (HsParTy name) = lparen <> pp name <> rparen
  pp (HsListTy name) = lbracket <> pp name <> rbracket

instance (Print name, IsSymOcc name) => Print (HsAppType name) where
  pp (HsAppPrefix (L _ a)) = pp a

instance (Print name, IsSymOcc name) => Print (ImportDecl name) where
  pp ImportDecl {..} =
    group $ string "import" <+>
    (if ideclQualified
       then string "qualified" <> space
       else empty) <>
    hang 2
      (pp ideclName <>
       (case ideclAs of
          Just n -> space <> string "as" <+> pp n
          Nothing -> empty) <>
       (case ideclHiding of
          Just (hiding, L _ names) ->
            line <> (if hiding then string "hiding" <> space else empty) <>
            tupled names
          Nothing -> empty))

instance (IsSymOcc a, Print a) => Print (IE a) where
  pp (IEVar n) = pp (fmap InfixOccName n)
  pp (IEThingAbs n) = pp (fmap InfixOccName n)
  pp (IEThingAll n) =
    pp (fmap InfixOccName n) <> lparen <> string ".." <> rparen

instance Print RdrName where
  pp (Unqual occName) = pp occName
  pp (Qual mod name) = pp mod <> dot <> pp name
  pp (Orig mod name) = pp mod <> dot <> pp name
  pp (Exact name) = pp (GHC.nameOccName name)

instance Print Module where
  pp = pp . moduleName

newtype InfixOccName name = InfixOccName name

class IsSymOcc a where
  isSymOcc :: a -> Bool

instance IsSymOcc GHC.OccName where
  isSymOcc = GHC.isSymOcc

instance IsSymOcc RdrName where
  isSymOcc = isSymOcc . rdrNameOcc

instance IsSymOcc b => IsSymOcc (GenLocated a b) where
  isSymOcc = isSymOcc . unLoc

instance (Print name, IsSymOcc name) => Print (InfixOccName name) where
  pp (InfixOccName occName)   | isSymOcc occName =   lparen <> pp occName <>
                                                     rparen
    | otherwise =   pp occName

instance Print GHC.OccName where
  pp n = string (pack (GHC.occNameString n))

instance Print ModuleName where
  pp = string . pack . GHC.moduleNameString

tupled :: (Print a) => [a] -> PrintM Doc
tupled = group . expandedTupled

expandedTupled :: (Print a) => [a] -> PrintM Doc
expandedTupled [] = lparen <> rparen
expandedTupled [a] = lparen <> pp a <> rparen
expandedTupled as =
  group $
  align
    (vcat
       (sequence $
        zipWith (<>) (lparen <> (space <|> empty) : repeat (comma <> space))
          (map pp as)) <$$>
     rparen)

singleLineTuple :: Print a => [a] -> PrintM Doc
singleLineTuple xs = lparen <> hsep (punctuate comma (mapM pp xs)) <> rparen

(<|>) :: Applicative m => m Doc -> m Doc -> m Doc
(<|>) = liftA2 (PP.<|>)

group :: PrintM Doc -> PrintM Doc
group child =
  do (doc, Any multiline) <- listen child
     if multiline
       then return doc
       else PPM.group (return doc)

(<$!>) :: PrintM Doc -> PrintM Doc -> PrintM Doc
(<$!>) l r =
  do tell (Any True)
     l <$> r

mandatoryLine :: PrintM Doc
mandatoryLine =
  do tell (Any True)
     line

lines :: PrintM [Doc] -> PrintM Doc
lines ds =
  do docs <- ds
     case docs of
       [] -> empty
       [x] -> pure x
       (x : xs) -> pure x <$!> lines (pure xs)

paragraphs :: PrintM [Doc] -> PrintM Doc
paragraphs = lines . punctuate line

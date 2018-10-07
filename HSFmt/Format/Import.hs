{-# language TupleSections #-}
{-# language ScopedTypeVariables #-}

module HSFmt.Format.Import ( imports ) where

import Debug.Trace
import Data.Data
import Data.Foldable
import Data.Function
import Data.List
import Data.Ord
import Language.Haskell.GHC.ExactPrint
import Data.Traversable (for)
import Data.Typeable
import GHC hiding (Phase, parseModule)
import Language.Haskell.GHC.ExactPrint.Types

import qualified Data.Map as Map

import HSFmt.Operation
import HSFmt.Util


-- | Group import statements together, but permit a single line to
-- separate groups of imports.

imports :: Op Transform
imports = topMost $ \lmod@(L l mod :: ParsedSource) -> do
  for_ (zip (hsmodImports mod) (tail (hsmodImports mod))) $ uncurry balanceComments
  case (hsmodDecls mod, last (hsmodImports mod)) of ([], _) -> return () ; ((d1:_), lastImport) -> balanceComments lastImport d1

  let
    imports =
      sortBy (comparing (unLoc . ideclName . unLoc)) ( hsmodImports mod )

  case imports of
    a:as -> do
      setEntryDPT a (DP (2, 0)) 
      for_ as $ \a -> setEntryDPT a (DP (1, 0))

  newOrder <-
    fixupImports imports

  return $
    L l (mod { hsmodImports = newOrder })

  where
    fixupImports
      :: [LImportDecl RdrName]
      -> Transform [LImportDecl RdrName]
    fixupImports is = do
      for is $ \i -> do
        mapAnnotation i $
          \ann ->
            ann
            { annEntryDelta =
                case annEntryDelta ann of
                  DP (col, _) -> DP (min col 2, 0)
            }

        names <-
          for (ideclHiding (unLoc i)) (uncurry fixupHiding)

        setEntryDPT (ideclName (unLoc i)) (DP (0, 1))

        return (fmap (\decl -> decl { ideclHiding = names }) i)

    fixupKW kw@(G AnnQualified) dp = (kw, DP (0, 1))
    fixupKW kw@(G AnnAs) dp = (kw, DP (0, 1))
    fixupKW kw@(G AnnHiding) dp = (kw, DP (1, 0))
    fixupKW kw@(G AnnOpenP) dp = (kw, DP (2, 0))
    fixupKW kw@(G AnnDotdot) dp = (kw, DP (0, 0))
    fixupKW kw@(G AnnCloseP) dp = (kw, DP (0, 0))
    fixupKW kw@(G AnnComma) dp = (kw, DP (0, 0))
    fixupKW other dp = (other, dp)

    fixupHiding
      :: Bool -> Located [LIE RdrName]
      -> Transform (Bool, Located [LIE RdrName])
    fixupHiding hiding names = do
      let
        sortedNames =
          fmap (sortBy (comparing (ieName . unLoc))) names

      let
        adjustKW g dp =
          (g, )
            $ case g of
                G AnnOpenP -> DP (0, 1)
                G AnnHiding -> DP (0, 1)
                G AnnCloseP -> DP (0, 0)
                _ -> dp

      -- The list of imports exactly follows the module name. We add necessary
      -- whitespace by moving the opening/closing parens and hiding keyword.
      setEntryDPT sortedNames (DP (0, 0))

      mapAnnotation sortedNames
        (\ann ->
          ann { annsDP = traceShowId $ map (uncurry adjustKW) $ traceShowId $ (annsDP ann)})

      for_ (unLoc sortedNames) $ \ie ->
        mapAnnotation ie
          (\ann ->
            ann { annsDP = map (uncurry fixupKW) (annsDP ann)})

      case unLoc sortedNames of
        (a:as) -> do
          setEntryDPT a (DP (0, 0))
          for_ as $ \x ->
            setEntryDPT x (DP (0, 1))
        _ -> return ()

      return (hiding, sortedNames)

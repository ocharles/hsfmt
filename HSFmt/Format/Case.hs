{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module HSFmt.Format.Case ( caseStatements ) where

import Debug.Trace
import Data.Foldable
import HSFmt.Operation
import GHC
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint
import HSFmt.Util

caseStatements :: Op Transform
caseStatements = bottomUp $ \case
  lcase@(L _ (HsCase e mg) :: LHsExpr GhcPs) -> do
    DP (_, col)  <- getEntryDPT lcase
    
    case unLoc ( mg_alts mg ) of
      a:as -> do
        setEntryDPT a (DP (1, col + 2))
        for_ as (`setEntryDPT` (DP (2, 0)))

  _ ->
    return ()

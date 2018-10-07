{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module HSFmt.Format.Do ( doStatements ) where

import Data.Foldable
import HSFmt.Operation
import GHC
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint

doStatements :: Op Transform
doStatements = bottomUp $ \case
  ldo@(L _ (HsDo _ statements _) :: LHsExpr GhcPs) ->
    case unLoc statements of
      a:as -> do
        -- setEntryDPT a (DP (1, 2))
        for_ as (`setEntryDPT` (DP (2, 0)))

  _ ->
    return ()

{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module HSFmt.Format.SwingGRHS ( swingGRHS ) where

import Debug.Trace
import Data.Foldable
import HSFmt.Operation
import GHC
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint
import HSFmt.Util


swingGRHS :: Op Transform
swingGRHS = bottomUp $ \case
  -- Do statements swing their children
  ( L _ ( GRHS _ body@( L _ ( HsDo _ statements _ ) ) ) :: Located ( GRHS RdrName ( LHsExpr GhcPs ) ) ) -> do
    setEntryDPT body (DP (0, 0))
    case unLoc statements of
      a:_ ->
        setEntryDPT a (DP (1, 0))

      _ ->
        return ()

  L _ ( GRHS _ body ) ->
    setEntryDPT body (DP (1, 2))

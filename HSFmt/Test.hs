module HSFmt.Test where

import GHC hiding (Phase, parseModule)
import Language.Haskell.GHC.ExactPrint

import HSFmt.Operation


import System.IO (  hPutStr )

formatFile fp op = do
  out <-
    parseModule fp

  case out of
    Left (_, e) ->
      fail $ "Could not parse file: " ++ e

    Right (annotations, parsed) ->
      let
        (transformed, (anns, _), log) =
          runTransform annotations (applyOp op parsed)

      in
        return $ exactPrint transformed anns

module HSFmt.Util ( mapAnnotation ) where

import Data.Data
import Data.Typeable
import Data.Foldable (for_)
import GHC hiding (Phase, parseModule)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types

import qualified Data.Map as Map

mapAnnotation
  :: (Data a)
  => Located a -> (Annotation -> Annotation) -> Transform ()
mapAnnotation ast f =
  let
    k = mkAnnKey ast

  in
    modifyAnnsT (Map.adjust f k)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Exact where

import Control.Monad
import Data.Data
import Data.Foldable
import qualified Data.Map as Map
import GHC hiding (Phase, parseModule)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types

-- formatFile :: FilePath -> Formatter -> IO String
formatFile fp op = do
  out <- parseModule fp
  case out of
    Left (_, e) ->
      fail $ "Could not parse file: " ++ e

    Right (annotations, parsed) -> do
      let (_, (anns, _), _) = runTransform annotations (everywhereM op parsed)
      return $ exactPrint parsed anns

everywhereBottomUp :: (Typeable a) => (a -> Transform x) -> Op Transform
everywhereBottomUp f =
  Op $ \x ->
    return (everywhereBottomUp f, Trans (\y -> y <$ for_ (cast y) f), x)

spaceSepOperators :: HsExpr RdrName -> Transform ()
spaceSepOperators (OpApp _ op _ r) = do
  setEntryDPT op (DP (1, 2))
  setEntryDPT r (DP (0, 1))
spaceSepOperators _ = return ()

newtype Trans m =
  Trans (forall a. Data a => a -> m a)

instance Monad m => Monoid (Trans m) where
  mempty = Trans return
  mappend (Trans f) (Trans g) = Trans (f >=> g)

newtype Op m =
  Op (forall a. Data a => a -> m (Op m, Trans m, a))

instance Monad m => Monoid (Op m) where
  mempty = Op (\a -> return (mempty, Trans return, a))
  mappend (Op l) (Op r) = Op $ \a -> do
    (l', x, a') <- (l a)
    (r', y, a'') <- (r a')
    return (mappend l' r', mappend x y, a'')


everywhereM
  :: (Data a, Monad m)
  => Op m
     -- ^ Top-down transformation.
  -> a
  -> m a
everywhereM (Op f) x = do
  (f', Trans g, x') <- f x
  x'' <- gmapM (everywhereM f') x'
  g x''

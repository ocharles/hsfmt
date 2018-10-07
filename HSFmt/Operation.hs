{-# language GADTs #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}

module HSFmt.Operation (Op, Trans, applyOp, topMost, bottomUp) where

import Data.Data (Data, gmapM)
import Data.Typeable ((:~:)( Refl ), Typeable, cast, eqT)
import Control.Monad ((>=>))
import Data.Foldable (for_)
import Language.Haskell.GHC.ExactPrint (Transform)


newtype Op m =
  Op (forall a. Data a => a -> m (Op m, Trans m, a))


instance Monad m => Monoid (Op m) where
  mempty =
    Op (\a -> return (mempty, Trans return, a))

  mappend (Op l) (Op r) = Op $ \a -> do
    (l', x, a') <- (l a)
    (r', y, a'') <- (r a')
    return (mappend l' r', mappend x y, a'')


newtype Trans m =
  Trans (forall a. Data a => a -> m a)


instance Monad m => Monoid (Trans m) where
  mempty =
    Trans return

  mappend (Trans f) (Trans g) =
    Trans (f >=> g)


-- | Run an operation.
applyOp
  :: (Data a, Monad m)
  => Op m
  -> a
  -> m a
applyOp (Op f) x = do
  (f', Trans g, x') <- f x
  x'' <- gmapM (applyOp f') x'
  g x''


-- | Apply a transformation bottom-up whenever the types match. 
bottomUp :: (Typeable a) => (a -> Transform x) -> Op Transform
bottomUp f =
  Op $ \x ->
    return (bottomUp f, Trans (\y -> y <$ for_ (cast y) f), x)


-- | Apply a transformation top-down whenever the types match.
topMost :: forall a. (Typeable a) => (a -> Transform a) -> Op Transform
topMost f =
  Op $ \(x :: t) ->
    case eqT @t @a of
      Nothing ->
        return (topMost f, mempty, x)

      Just Refl -> do
        x' <- f x
        return (mempty, mempty, x')

module GameOfLife.Repa where

import Data.Array.Repa (Array, D, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Unsafe qualified as Repa
import Data.Coerce (coerce)
import GameOfLife.Base (GameOfLife (..))
import GameOfLife.Life (Life (..), joinLife, liveOrDie)
import GameOfLife.Life qualified as L
import GameOfLife.Patterns (Pattern)

newtype RepaGoL = R (Array U DIM2 Life)

extract :: RepaGoL -> Array U DIM2 Life
extract = coerce

toTuple :: DIM2 -> (Int, Int)
toTuple (Z :. j :. i) = (i, j)

instance GameOfLife RepaGoL where
  {-# INLINE empty #-}
  empty :: (Int, Int) -> RepaGoL
  empty (w, h) = R . Repa.computeS $ Repa.delay $ Repa.fromListUnboxed (Z :. h :. w) $ take (w * h) $ X <$ [0 :: Int ..]

  {-# INLINE fromPattern #-}
  fromPattern :: Pattern -> RepaGoL
  fromPattern p = R . Repa.computeS $ Repa.delay $ Repa.fromListUnboxed (Z :. length p :. length (head p)) $ concat p

  {-# INLINE size #-}
  size :: RepaGoL -> (Int, Int)
  size = toTuple . Repa.extent . coerce

  {-# INLINE joinO #-}
  joinO :: (Int, Int) -> RepaGoL -> RepaGoL -> RepaGoL
  joinO (wo, ho) (R xs) (R ys) = R . Repa.computeS $ Repa.traverse2 xs ys const (\f g i -> safe g (toTuple i) `joinLife` f i)
    where
      safe g (i, j) = safeIndex (toTuple $ Repa.extent ys) g (i - wo, j - ho)

  {-# INLINE flipX #-}
  flipX :: RepaGoL -> RepaGoL
  flipX (R xs) = R . Repa.computeS $ Repa.traverse xs id (\f (Z :. j :. i) -> f (Z :. (h - j - 1) :. i))
    where
      (_, h) = toTuple $ Repa.extent xs

  {-# INLINE flipY #-}
  flipY :: RepaGoL -> RepaGoL
  flipY (R xs) = R . Repa.computeS $ Repa.traverse xs id (\f (Z :. j :. i) -> f (Z :. j :. (w - i - 1)))
    where
      (w, _) = toTuple $ Repa.extent xs

  {-# INLINE transpose #-}
  transpose :: RepaGoL -> RepaGoL
  transpose = coerce . Repa.computeS . Repa.transpose . coerce

  -- Does not expand, should stay inside size
  {-# INLINE forward #-}
  forward :: Int -> RepaGoL -> RepaGoL
  forward 0 = id
  forward n = forward (n - 1) . coerce . Repa.computeS . processLives . coerce

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !xs !index = toTuple $ Repa.fromIndex (Repa.extent xs) index

{-# INLINE safeIndex #-}
safeIndex :: (Int, Int) -> (DIM2 -> Life) -> (Int, Int) -> Life
safeIndex (!w, !h) !m (!i, !j) = if i < 0 || j < 0 || i >= w || j >= h then X else m (Z :. j :. i)

{-# INLINE processLives #-}
processLives :: (Source r Life) => Array r DIM2 Life -> Array D DIM2 Life
processLives !xs = Repa.unsafeTraverse xs id $ processLife $ toTuple $ Repa.extent xs

{-# INLINE processLife #-}
processLife :: (Int, Int) -> (DIM2 -> Life) -> DIM2 -> Life
processLife si !f (Z :. !j :. !i) = liveOrDie c (n + ne + e + se + s + sw + w + nw)
  where
    !indexer = L.coerce . safeIndex si f
    !c = f (Z :. j :. i)
    !n = indexer (i, j - 1)
    !ne = indexer (i + 1, j - 1)
    !e = indexer (i + 1, j)
    !se = indexer (i + 1, j + 1)
    !s = indexer (i, j + 1)
    !sw = indexer (i - 1, j + 1)
    !w = indexer (i - 1, j)
    !nw = indexer (i - 1, j - 1)

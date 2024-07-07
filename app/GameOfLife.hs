module GameOfLife where

import Data.Array.Repa (Array, D, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Unsafe qualified as Repa
import Patterns (Pattern)
import Types (From (from), Life (..))
import Types qualified as T

class GameOfLife g where
  empty :: (Int, Int) -> g

  fromPattern :: Pattern -> g

  size :: g -> (Int, Int)

  flipX :: g -> g

  flipY :: g -> g

  transpose :: g -> g

  joinO ::
    (Int, Int) -> -- Offset to insert new life
    g -> -- Game containing existing life, to insert to
    g -> -- Game containing new life
    g

  -- Goes forward n iterations in game of life
  forward :: Int -> g -> g

  {-# INLINE move #-}
  move :: (Int, Int) -> g -> g
  move (x, y) b = joinO (x, y) (empty (x + w, y + h)) b
    where
      (w, h) = size b

  {-# INLINE expand #-}
  expand :: (Int, Int) -> g -> g
  expand = join . empty

  {-# INLINE fits #-}
  fits :: g -> g -> Bool
  fits xs ys = w <= w' && h <= h'
    where
      (w, h) = size xs
      (w', h') = size ys

  {-# INLINE join #-}
  join :: g -> g -> g
  join xs ys
    | fits xs ys = joinO (0, 0) ys xs
    | fits ys xs = joinO (0, 0) xs ys
    | otherwise = error $ "Invalid Join, array sizes are " ++ show (size xs) ++ " and " ++ show (size ys)

  -- alias for join
  {-# INLINE (<.>) #-}
  (<.>) :: g -> g -> g
  (<.>) = join

type Board = Array U DIM2 Life

instance GameOfLife Board where
  {-# INLINE empty #-}
  empty :: (Int, Int) -> Board
  empty (w, h) = Repa.computeS $ Repa.delay $ Repa.fromListUnboxed (Z :. h :. w) $ take (w * h) $ X <$ [0 :: Int ..]

  {-# INLINE fromPattern #-}
  fromPattern :: Pattern -> Board
  fromPattern p = Repa.computeS $ Repa.delay $ Repa.fromListUnboxed (Z :. length p :. length (head p)) $ concat p

  {-# INLINE size #-}
  size :: Board -> (Int, Int)
  size xs = (w, h)
    where
      (Z :. h :. w) = Repa.extent xs

  {-# INLINE joinO #-}
  joinO :: (Int, Int) -> Board -> Board -> Board
  joinO (wo, ho) xs ys = Repa.computeS $ Repa.traverse2 xs ys const (\f g i -> safe g i `T.joinLife` f i)
    where
      (Z :. h :. w) = Repa.extent ys
      safe g (Z :. j :. i) = safeIndex (w, h) g (i - wo, j - ho)

  {-# INLINE flipX #-}
  flipX :: Board -> Board
  flipX b = Repa.computeS $ Repa.traverse b id (\f (Z :. j :. i) -> f (Z :. (h - j - 1) :. i))
    where
      (Z :. h :. _) = Repa.extent b

  {-# INLINE flipY #-}
  flipY :: Board -> Board
  flipY b = Repa.computeS $ Repa.traverse b id (\f (Z :. j :. i) -> f (Z :. j :. (w - i - 1)))
    where
      (Z :. _ :. w) = Repa.extent b

  {-# INLINE transpose #-}
  transpose :: Board -> Board
  transpose = Repa.computeS . Repa.transpose

  -- Todo, prevent error if not inside array
  {-# INLINE forward #-}
  forward :: Int -> Board -> Board
  forward 0 = id
  forward n = forward (n - 1) . Repa.computeS . processLives

{-# INLINE safeIndex #-}
safeIndex :: (Int, Int) -> (DIM2 -> Life) -> (Int, Int) -> Life
safeIndex (!w, !h) !m (!i, !j) = if i < 0 || j < 0 || i >= w || j >= h then X else m (Z :. j :. i)

{-# INLINE processLives #-}
processLives :: (Source r Life) => Array r DIM2 Life -> Array D DIM2 Life
processLives !xs = Repa.unsafeTraverse xs id (processLife (w, h))
  where
    (Z :. h :. w) = Repa.extent xs

{-# INLINE processLife #-}
processLife :: (Int, Int) -> (DIM2 -> Life) -> DIM2 -> Life
processLife si !f (Z :. !j :. !i) = liveOrDie c (n + ne + e + se + s + sw + w + nw)
  where
    !indexer = Types.from . safeIndex si f
    !c = f (Z :. j :. i)
    !n = indexer (i, j - 1)
    !ne = indexer (i + 1, j - 1)
    !e = indexer (i + 1, j)
    !se = indexer (i + 1, j + 1)
    !s = indexer (i, j + 1)
    !sw = indexer (i - 1, j + 1)
    !w = indexer (i - 1, j)
    !nw = indexer (i - 1, j - 1)

{-# INLINE liveOrDie #-}
liveOrDie :: Life -> Int -> Life
liveOrDie _ 3 = O
liveOrDie O 2 = O
liveOrDie _ _ = X
module GameOfLife where

import Data.Array.Repa (Array, D, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Unsafe qualified as Repa
import Patterns (Pattern (..))
import Types (From (from), Life (..))
import Types qualified as T

class GameOfLife g where
  empty :: (Int, Int) -> g

  fromPattern :: Pattern -> g

  size :: g -> (Int, Int)

  fits :: g -> g -> (Int, Int) -> Bool

  unsafeJoin ::
    g -> -- Game containing existing life, to insert to
    g -> -- Game containing new life
    (Int, Int) -> -- Offset to insert new life
    g

  -- Guarantees that the game will be expanded to be able to house all life
  join ::
    g -> -- Game containing existing life, to insert to
    g -> -- Game containing new life
    (Int, Int) -> -- Offset to insert new life
    g

  expand :: g -> (Int, Int) -> g

  -- Goes forward n iterations in game of life
  forward :: Int -> g -> g

type Board = Array D DIM2 Life

instance GameOfLife Board where
  empty :: (Int, Int) -> Board
  empty (w, h) = Repa.delay $ Repa.fromListUnboxed (Z :. h :. w) $ take (w * h) $ X <$ [0 :: Int ..]

  fromPattern :: Pattern -> Board
  fromPattern (Pattern (w, h) p) = Repa.delay $ Repa.fromListUnboxed (Z :. h :. w) $ concat p

  size :: Board -> (Int, Int)
  size xs = (w, h)
    where
      (Z :. h :. w) = Repa.extent xs

  -- Expects current board to be smaller than the requested size
  expand :: Board -> (Int, Int) -> Board
  expand xs s' = join (empty s') xs (0, 0)

  fits :: Board -> Board -> (Int, Int) -> Bool
  fits xs ys (wo, ho) = not (w < w' + wo || h < h' + ho)
    where
      (w, h) = size xs
      (w', h') = size ys

  join :: Board -> Board -> (Int, Int) -> Board
  join xs ys (wo, ho) = if fits xs ys (wo, ho) then unsafeJoin xs ys (wo, ho) else unsafeJoin (expand xs (w + wo, h + ho)) ys (wo, ho)
    where
      (w, h) = size xs

  unsafeJoin :: Board -> Board -> (Int, Int) -> Board
  unsafeJoin xs ys (wo, ho) = Repa.traverse2 xs ys const (\f g i -> safe g i `T.joinLife` f i)
    where
      (Z :. h :. w) = Repa.extent ys
      safe g (Z :. j :. i) = safeIndex (w, h) g (i - wo, j - ho)

  forward :: Int -> Board -> Board
  forward 0 = id
  forward n = forward (n - 1) . processLives

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
processLife size !f (Z :. !j :. !i) = liveOrDie c (n + ne + e + se + s + sw + w + nw)
  where
    !indexer = Types.from . safeIndex size f
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
module GameOfLife.Base where

import GameOfLife.Patterns (Pattern)

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

  {-# INLINE rotate #-}
  rotate :: g -> g
  rotate = flipX . flipY

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
    | otherwise = joinO (0, 0) (expand newSize xs) (expand newSize ys)
    where
      newSize = zipWithT2 max (size xs) (size ys)

  -- alias for join
  {-# INLINE (<.>) #-}
  (<.>) :: g -> g -> g
  (<.>) = join

{-# INLINE zipWithT2 #-}
zipWithT2 :: (a -> a -> a) -> (a, a) -> (a, a) -> (a, a)
zipWithT2 f (i, j) (i', j') = (f i i', f j j')

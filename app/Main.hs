module Main where

import Data.Array.Repa (Array, D, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector qualified as Repa
import Data.Array.Repa.Unsafe qualified as Repa
import Foreign.C (CInt)
import Patterns qualified as P
import SDL qualified
import Sdl qualified
import Types (Life (..), joinLife)
import Types qualified

matrix :: Array U DIM2 Life
matrix = Types.from P.reflector

resolution :: (Int, Int)
resolution = (1920, 1080)

pixelSize :: Int
pixelSize = 1

width :: Int
width = fst resolution `div` pixelSize

height :: Int
height = snd resolution `div` pixelSize

cellCount :: Int
cellCount = height * width

emptyMatrix :: Array U DIM2 Life
emptyMatrix = Repa.fromListUnboxed (Z :. height :. width) $ take cellCount $ X <$ [0 :: Int ..]

addLives ::
  Array U DIM2 Life -> -- Array containing existing life, to insert to
  Array U DIM2 Life -> -- Array containing new life
  (Int, Int) -> -- Offset to insert new life
  Array D DIM2 Life
addLives xs ys (wo, ho) = Repa.traverse2 xs ys const (\f g i -> safe g i `joinLife` f i)
  where
    (Z :. h :. w) = Repa.extent ys
    safe g (Z :. j :. i) = safeIndex (w, h) g (i - wo, j - ho)

board :: Array U DIM2 Life
board = Repa.computeS $ addLives emptyMatrix matrix (20, 20)

main :: IO ()
main = Sdl.withSdl "Functional Life" resolution $ flip runLife board

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO ()
runLife g b = do
  Sdl.clearScreen g

  alive <- Repa.selectP (Types.from . Repa.linearIndex b) (coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (rectangle pixelSize) alive

  Sdl.fillRectangles g $ Repa.toVector rectangles

  b' <- Repa.computeUnboxedP $ processLives b

  Sdl.present g

  runLife g b'

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !b !index = (i, j)
  where
    (Z :. !j :. !i) = Repa.fromIndex (Repa.extent b) index

{-# INLINE rectangle #-}
rectangle :: Int -> (Int, Int) -> SDL.Rectangle CInt
rectangle !ps (!x, !y) = SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (x * ps) (y * ps)) (fromIntegral <$> SDL.V2 ps ps)

{-# INLINE processLives #-}
processLives :: (Source r Life) => Array r DIM2 Life -> Array D DIM2 Life
processLives !xs = Repa.unsafeTraverse xs id processLife

{-# INLINE processLife #-}
processLife :: (DIM2 -> Life) -> DIM2 -> Life
processLife !f (Z :. !j :. !i) = liveOrDie c (n + ne + e + se + s + sw + w + nw)
  where
    !indexer = Types.from . safeIndex (width, height) f
    !c = f (Z :. j :. i)
    !n = indexer (i, j - 1)
    !ne = indexer (i + 1, j - 1)
    !e = indexer (i + 1, j)
    !se = indexer (i + 1, j + 1)
    !s = indexer (i, j + 1)
    !sw = indexer (i - 1, j + 1)
    !w = indexer (i - 1, j)
    !nw = indexer (i - 1, j - 1)

{-# INLINE safeIndex #-}
safeIndex :: (Int, Int) -> (DIM2 -> Life) -> (Int, Int) -> Life
safeIndex (!w, !h) !m (!i, !j) = if i < 0 || j < 0 || i >= w || j >= h then X else m (Z :. j :. i)

{-# INLINE liveOrDie #-}
liveOrDie :: Life -> Int -> Life
liveOrDie _ 3 = O
liveOrDie O 2 = O
liveOrDie _ _ = X
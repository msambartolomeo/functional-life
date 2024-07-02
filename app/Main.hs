module Main where

import Data.Array.Repa (Array, D, DIM1, DIM2, Shape, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector (V)
import Data.Array.Repa.Repr.Vector qualified as Repa
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Foreign.C (CInt)
import SDL qualified
import Sdl qualified
import Types (Life)

raw :: [Life]
raw =
  [ 
    True, True,  True,  True,  True,
    True, False, False, False, True,
    True, False, False, False, True,
    True, False, False, False, True,
    True, True,  True,  True,  True
  ]

size :: Int
size = 5

matrix :: Array U DIM2 Life
matrix = Repa.fromListUnboxed (Z :. size :. size) raw

main :: IO ()
main = do
  Sdl.withSdl
    "Functional Life"
    (1920, 1080)
    ( \g -> do
        Sdl.clearScreen g

        rectangles <- computeVector $ getRectanglesFromLife matrix

        Sdl.fillRectangles g $ filterMaybe $ Repa.toVector rectangles

        Sdl.present g

        SDL.delay 2000
    )

filterMaybe :: Vector (Maybe a) -> Vector a
filterMaybe = Vector.mapMaybe id

computeVector :: (Shape a) => Array D a b -> IO (Array V a b)
computeVector = Repa.computeP

getRectanglesFromLife :: Array U DIM2 Life -> Array D DIM1 (Maybe (SDL.Rectangle CInt))
getRectanglesFromLife arr = Repa.reshape (Z :. size * size) (Repa.traverse arr id (\l (Z :. i :. j) -> rectangle (l (Z :. i :. j)) (i, j)))

pixelSize :: Int
pixelSize = 10

rectangle :: Life -> (Int, Int) -> Maybe (SDL.Rectangle CInt)
rectangle True (x, y) = Just $ SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (y * pixelSize) (x * pixelSize)) (fromIntegral <$> SDL.V2 pixelSize pixelSize)
rectangle False _ = Nothing

module Main where

import Data.Array.Repa (Array, D, DIM1, DIM2, Shape, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector (V)
import Data.Array.Repa.Repr.Vector qualified as Repa
import Data.Vector qualified as Vector
import Debug.Trace qualified as Debug
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
size = 3

matrix :: Array U DIM2 Life
matrix = Repa.fromListUnboxed (Z :. size :. size) raw

resolution :: (Int, Int)
resolution = (1920, 1080)

pixelSize :: Int
pixelSize = 10

width :: Int
width = snd resolution `div` pixelSize

height :: Int
height = fst resolution `div` pixelSize

cellCount :: Int
cellCount = height * width

emptyMatrix :: Array U DIM2 Life
emptyMatrix = Repa.fromListUnboxed (Z :. height :. width) $ take cellCount $ False <$ [0 :: Int ..]

addLife :: Array U DIM2 Life -> Array U DIM2 Life -> Array D DIM2 Life
addLife xs ys | Debug.trace (show xs ++ "\n" ++ show ys) False = undefined
addLife xs ys =
  Repa.traverse2
    xs
    ys
    (\_ x -> x)
    func

func :: (DIM2 -> Life) -> (((Z :. Int) :. Int) -> Bool) -> ((Z :. Int) :. Int) -> Bool
func _ _ (Z :. i :. j) | Debug.trace ("(" ++ show i ++ ", " ++ show j ++ ")") False = undefined
func f g (Z :. i :. j) =
  let x = safeIndex f i j
      y = g (Z :. i :. j)
   in x || y

board :: Array U DIM2 Life
board = Repa.computeS $ addLife matrix emptyMatrix

timeBetweenFrames :: Double
timeBetweenFrames = 0.5

main :: IO ()
main = Sdl.withSdl "Functional Life" resolution $ flip runLife board

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO ()
runLife g b = do
  Sdl.clearScreen g

  rectangles <- computeVector $ getRectanglesFromLife b

  print $ length $ Vector.catMaybes $ Repa.toVector rectangles

  Sdl.fillRectangles g $ Vector.catMaybes $ Repa.toVector rectangles

  b' <- Repa.computeUnboxedP $ processLives b

  SDL.delay 2000

  Sdl.present g

  runLife g b'

computeVector :: (Shape a) => Array D a b -> IO (Array V a b)
computeVector = Repa.computeP

getRectanglesFromLife :: Array U DIM2 Life -> Array D DIM1 (Maybe (SDL.Rectangle CInt))
getRectanglesFromLife arr = Repa.reshape (Z :. cellCount) (Repa.traverse arr id (\l (Z :. i :. j) -> rectangle (l (Z :. i :. j)) (i, j)))

rectangle :: Life -> (Int, Int) -> Maybe (SDL.Rectangle CInt)
rectangle True (x, y) = Just $ SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (y * pixelSize) (x * pixelSize)) (fromIntegral <$> SDL.V2 pixelSize pixelSize)
rectangle False _ = Nothing

processLives :: Array U DIM2 Life -> Array D DIM2 Life
processLives xs = Repa.traverse xs id processLife

processLife :: (DIM2 -> Life) -> DIM2 -> Life
processLife f (Z :. i :. j) = liveOrDie c $ (length . filter id) [n, ne, e, se, s, sw, w, nw]
  where
    c  = f (Z :. i :. j)
    n  = safeIndex f i       (j - 1)
    ne = safeIndex f (i + 1) (j - 1)
    e  = safeIndex f (i + 1) j
    se = safeIndex f (i + 1) (j + 1)
    s  = safeIndex f i       (j + 1)
    sw = safeIndex f (i - 1) (j + 1)
    w  = safeIndex f (i - 1) j
    nw = safeIndex f (i - 1) (j - 1)

safeIndex :: (DIM2 -> Life) -> Int -> Int -> Life
-- safeIndex _ i j | Debug.trace ("(" ++ show i ++ ", " ++ show j ++ ")" ) False  = undefined
safeIndex m i j = not (i < 0 || j < 0 || i > width || j > height) && m (Z :. i :. j)

liveOrDie :: Life -> Int -> Life
-- liveOrDie b    n | Debug.trace ("This life is " ++ show b ++ " and has " ++ show n ) False  = undefined
liveOrDie _    3 = True
liveOrDie True 2 = True
liveOrDie _    _ = False

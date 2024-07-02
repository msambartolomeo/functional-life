module Main where

-- import Debug.Trace qualified as Debug
import Data.Array.Repa (Array, D, DIM1, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector qualified as Repa
import Data.Vector qualified as Vector
import Foreign.C (CInt)
import SDL qualified
import Sdl qualified
import Types (Life)

raw :: [Life]
raw =
  [ 
    False, False, True,
    True,  False, True,
    False, True,  True
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
width = fst resolution `div` pixelSize

height :: Int
height = snd resolution `div` pixelSize

cellCount :: Int
cellCount = height * width

emptyMatrix :: Array U DIM2 Life
emptyMatrix = Repa.fromListUnboxed (Z :. height :. width) $ take cellCount $ False <$ [0 :: Int ..]

addLives :: Array U DIM2 Life -> Array U DIM2 Life -> Array D DIM2 Life
addLives xs ys = Repa.traverse2 xs ys const (\f g i -> safe g i || f i)
  where
    (Z :. h :. w) = Repa.extent ys
    safe g (Z :. j :. i) = safeIndex (w, h) g i j

board :: Array U DIM2 Life
board = Repa.computeS $ addLives emptyMatrix matrix

main :: IO ()
main = Sdl.withSdl "Functional Life" resolution $ flip runLife board

runLife :: (Source d Life) => Sdl.Sdl -> Array d DIM2 Life -> IO ()
runLife g b = do
  Sdl.clearScreen g

  rectangles <- Repa.computeVectorP $ getRectanglesFromLife b

  Sdl.fillRectangles g $ Vector.catMaybes $ Repa.toVector rectangles

  b' <- Repa.computeUnboxedP $ processLives b

  SDL.delay 50

  Sdl.present g

  runLife g b'

getRectanglesFromLife :: (Source d Life) => Array d DIM2 Life -> Array D DIM1 (Maybe (SDL.Rectangle CInt))
getRectanglesFromLife arr = Repa.reshape (Z :. cellCount) (Repa.traverse arr id (\l (Z :. j :. i) -> rectangle (l (Z :. j :. i)) (i, j)))

rectangle :: Life -> (Int, Int) -> Maybe (SDL.Rectangle CInt)
rectangle True (x, y) = Just $ SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (y * pixelSize) (x * pixelSize)) (fromIntegral <$> SDL.V2 pixelSize pixelSize)
rectangle False _ = Nothing

processLives :: (Source d Life) => Array d DIM2 Life -> Array D DIM2 Life
processLives xs = Repa.traverse xs id processLife

processLife :: (DIM2 -> Life) -> DIM2 -> Life
-- processLife f (Z :. j :. i) | Debug.trace ("(" ++ show i ++ ", " ++ show j ++ ") :: " ++ show (f (Z :. j :. i))) False  = undefined
processLife f (Z :. j :. i) = liveOrDie c $ (length . filter id) [n, ne, e, se, s, sw, w, nw]
  where
    indexer = safeIndex (width, height) f
    c = f (Z :. j :. i)
    n = indexer i (j - 1)
    ne = indexer (i + 1) (j - 1)
    e = indexer (i + 1) j
    se = indexer (i + 1) (j + 1)
    s = indexer i (j + 1)
    sw = indexer (i - 1) (j + 1)
    w = indexer (i - 1) j
    nw = indexer (i - 1) (j - 1)

safeIndex :: (Int, Int) -> (DIM2 -> Life) -> Int -> Int -> Life
-- safeIndex _ _ i j      | Debug.trace ("(" ++ show i ++ ", " ++ show j ++ ")") False  = undefined
safeIndex (w, h) m i j = not (i < 0 || j < 0 || i >= w || j >= h) && m (Z :. j :. i)

liveOrDie :: Life -> Int -> Life
-- liveOrDie b    n | Debug.trace ("This life is " ++ show b ++ " and has " ++ show n ) False  = undefined
liveOrDie _ 3 = True
liveOrDie True 2 = True
liveOrDie _ _ = False

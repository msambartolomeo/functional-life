module Main where

import Data.Array.Repa (Array, D, DIM1, DIM2, Shape, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector (V)
import Data.Array.Repa.Repr.Vector qualified as Repa
import Data.Text (Text)
import Data.Vector qualified as Vector
import Data.Word (Word8)
import Foreign.C (CInt)
import SDL (($=))
import SDL qualified

-- data Life = Alive | Dead
type Life = Bool

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
  SDL.initializeAll
  w <- createWindow "Functional Life" (640, 480)
  SDL.showWindow w
  r <- createRenderer w

  clearScreen r

  SDL.rendererDrawColor r $= from Black

  rectangles <- computeVector (getRectanglesFromLife matrix)

  let vector = Repa.toVector rectangles

  let rectangles2 = Vector.mapMaybe id vector

  let vector2 = Vector.convert rectangles2

  SDL.fillRects r vector2

  SDL.present r

  SDL.delay 2000

  SDL.destroyRenderer r
  SDL.destroyWindow w
  SDL.quit

createWindow :: Text -> (Int, Int) -> IO SDL.Window
createWindow t s = SDL.createWindow t $ windowConfig s

windowConfig :: (Int, Int) -> SDL.WindowConfig
windowConfig (x, y) = SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> SDL.V2 x y}

createRenderer :: SDL.Window -> IO SDL.Renderer
createRenderer w = SDL.createRenderer w 0 $ SDL.RendererConfig SDL.AcceleratedRenderer False

clearScreen :: SDL.Renderer -> IO ()
clearScreen r = SDL.rendererDrawColor r $= from White >> SDL.clear r

computeVector :: (Shape a) => Array D a b -> IO (Array V a b)
computeVector = Repa.computeP

class From a b where
  from :: a -> b

data Color = White | Black

instance From Color (SDL.V4 Word8) where
  from :: Color -> SDL.V4 Word8
  --                  R   G   B   a
  from White = SDL.V4 255 255 255 255
  from Black = SDL.V4 0 0 0 255

getRectanglesFromLife :: Array U DIM2 Life -> Array D DIM1 (Maybe (SDL.Rectangle CInt))
getRectanglesFromLife arr = Repa.reshape (Z :. size * size) (Repa.traverse arr id (\l (Z :. i :. j) -> rectangle (l (Z :. i :. j)) (i, j)))

pixelSize :: Int
pixelSize = 10

rectangle :: Life -> (Int, Int) -> Maybe (SDL.Rectangle CInt)
rectangle True (x, y) = Just $ SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (y * pixelSize) (x * pixelSize)) (fromIntegral <$> SDL.V2 pixelSize pixelSize)
rectangle False _ = Nothing
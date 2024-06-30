module Main where

import Data.Text (Text)
import Data.Word (Word8)
import SDL qualified

main :: IO ()
main = do
  SDL.initializeAll
  w <- createWindow "Functional Life" (640, 480)
  SDL.showWindow w
  r <- createRenderer w

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

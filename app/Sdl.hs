module Sdl where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Foreign.C (CInt)
import SDL (($=))
import SDL qualified
import System.Exit (exitSuccess)
import Types (Color (..), From (..))

data Sdl = Sdl SDL.Window SDL.Renderer

newSdl :: Text -> (Int, Int) -> IO Sdl
newSdl title size = do
  SDL.initializeAll
  w <- createWindow title size
  SDL.showWindow w
  r <- createRenderer w
  return $ Sdl w r

createWindow :: Text -> (Int, Int) -> IO SDL.Window
createWindow t s = SDL.createWindow t $ windowConfig s

windowConfig :: (Int, Int) -> SDL.WindowConfig
windowConfig (x, y) = SDL.defaultWindow {SDL.windowInitialSize = fromIntegral <$> SDL.V2 x y}

createRenderer :: SDL.Window -> IO SDL.Renderer
createRenderer w = SDL.createRenderer w 0 $ SDL.RendererConfig SDL.AcceleratedRenderer False

dropSdl :: Sdl -> IO ()
dropSdl (Sdl w r) = do
  SDL.destroyRenderer r
  SDL.destroyWindow w
  SDL.quit

-- Runs a function using and freeing SDL context using bracket
withSdl :: Text -> (Int, Int) -> (Sdl -> IO a) -> IO a
withSdl title size = bracket (newSdl title size) dropSdl

-- SDL Functions

handleEvent :: SDL.EventPayload -> IO ()
handleEvent SDL.QuitEvent = exitSuccess
handleEvent _ = return ()

mainLoop :: Sdl -> (Sdl -> t -> IO t) -> t -> IO b
mainLoop s f x = do
  SDL.pollEvents >>= foldr ((=<<) . return . handleEvent . SDL.eventPayload) (return ())

  clearScreen s

  x' <- f s x

  Sdl.present s

  -- SDL.delay 5000

  mainLoop s f x'

clearScreen :: Sdl -> IO ()
clearScreen (Sdl _ r) = SDL.rendererDrawColor r $= from White >> SDL.clear r

fillRectangles :: Sdl -> Vector (SDL.Rectangle CInt) -> IO ()
fillRectangles (Sdl _ r) rects = SDL.rendererDrawColor r $= from Black >> SDL.fillRects r (Vector.convert rects)

present :: Sdl -> IO ()
present (Sdl _ r) = SDL.present r

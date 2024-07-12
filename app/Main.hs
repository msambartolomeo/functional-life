module Main where

import Data.Array.Repa (Array, DIM2, U)
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector qualified as Repa
import Data.Maybe (fromMaybe)
import Fonts qualified
import GameOfLife.Life (Life, coerce)
import GameOfLife.Repa qualified as GoL
import Printer qualified
import ReadArgs (readArgs)
import Sdl (Sdl)
import Sdl qualified

main :: IO ()
main = do
  (text :: String, resolution :: Maybe (Int, Int)) <- readArgs

  Sdl.withSdl "Functional Life" (fromMaybe (1920, 1080) resolution) $ runPrinter text

runPrinter :: String -> Sdl -> IO ()
runPrinter text sdl = Sdl.mainLoop sdl runLife $ (GoL.extract . Printer.buildPrinters . Fonts.joinRepresentation . Fonts.fromString) text

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO (Array U DIM2 Life)
runLife g b = do
  aliveCoordinates <- Repa.selectP (coerce . Repa.linearIndex b) (GoL.coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (Sdl.rectangle 1) aliveCoordinates

  Sdl.fillRectangles g $ Repa.toVector rectangles

  Repa.computeP $ GoL.processLives b

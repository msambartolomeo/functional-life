module Main where

import Data.Array.Repa (Array, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector qualified as Repa
import Data.Maybe (fromMaybe)
import Fonts (Font, fontLines, fromString)
import GameOfLife (GameOfLife ((<.>)), processLives)
import GameOfLife qualified as GoL
import Printer qualified
import ReadArgs (readArgs)
import Sdl qualified
import Types (Life)
import Types qualified

main :: IO ()
main = do
  (text :: String, resolution :: Maybe (Int, Int)) <- readArgs

  Sdl.withSdl "Functional Life" (fromMaybe (1920, 1080) resolution) (\sdl -> Sdl.mainLoop sdl runLife $ GoL.move (1000, 10) $ buildLife $ fromString text)

buildLife :: (GameOfLife g) => [Font] -> g
buildLife = foldr (\(i, bs) gol -> GoL.move (i * 92, i * 21) (Printer.createPrinter i bs) <.> gol) (GoL.empty (0, 0)) . zip [0 :: Int ..] . fontLines

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO (Array U DIM2 Life)
runLife g b = do
  aliveCoordinates <- Repa.selectP (Types.from . Repa.linearIndex b) (coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (Sdl.rectangle 1) aliveCoordinates

  Sdl.fillRectangles g $ Repa.toVector rectangles

  Repa.computeP $ processLives b

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !b !index = (i, j)
  where
    (Z :. !j :. !i) = Repa.fromIndex (Repa.extent b) index

module Main where

import Data.Array.Repa (Array, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector qualified as Repa
import Fonts (Font (A, H, L, O), fontLines)
import GameOfLife (processLives)
import GameOfLife qualified as GoL
import Printer qualified
import Sdl qualified
import Types (Life)
import Types qualified

resolution :: (Int, Int)
resolution = (1920, 1080)

word :: [Font]
word = [H, O, L, A]

-- dots = take 41 $ True <$ [0 :: Int ..]

main :: IO ()
main = do
  Sdl.withSdl "Functional Life" resolution (\sdl -> Sdl.mainLoop sdl runLife $ GoL.move (300, 150) $ Printer.createPrinter $ head $ fontLines word)

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO (Array U DIM2 Life)
runLife g b = do
  aliveCoordinates <- Repa.selectP (Types.from . Repa.linearIndex b) (coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (Sdl.rectangle pixelSize) aliveCoordinates

  Sdl.fillRectangles g $ Repa.toVector rectangles

  Repa.computeP $ processLives b
  where
    pixelSize = 2

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !b !index = (i, j)
  where
    (Z :. !j :. !i) = Repa.fromIndex (Repa.extent b) index

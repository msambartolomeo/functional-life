module Main where

import Data.Array.Repa (Array, DIM2, Source, U, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa
import Data.Array.Repa.Repr.Vector qualified as Repa
import Foreign.C (CInt)
import GameOfLife (Board, GameOfLife ((<.>)))
import GameOfLife qualified as GoL
import Patterns qualified as P
import SDL qualified
import Sdl qualified
import Types (Life (..))
import Types qualified

doubleOsci :: Board
doubleOsci = top_refector <.> GoL.move (8, 21) bottom_reflector
  where
    top_refector = GoL.fromPattern P.reflector
    bottom_reflector = GoL.flipX top_refector

stopperOsci :: Board
stopperOsci = GoL.fromPattern P.reflector <.> GoL.move (9, 27) (GoL.fromPattern P.box)

reflectorNW :: Board
reflectorNW = GoL.flipY stopperOsci

reflectorSW :: Board
reflectorSW = (GoL.transpose . GoL.flipY) stopperOsci

reflectorSE :: Board
reflectorSE = doubleOsci

reflectorNE :: Board
reflectorNE = GoL.flipY stopperOsci

reflectorPrinter :: Board
reflectorPrinter = stopperOsci

allRef :: Board
allRef = createBoard reflectorSE <.> GoL.move (25, 0) reflectorNE <.> GoL.move (50, 0) reflectorNW <.> GoL.move (75, 0) reflectorPrinter <.> GoL.move (50, 50) reflectorSW

createBoard :: Board -> Board
createBoard = GoL.join (GoL.empty resolution) . GoL.move (2, 2)

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

main :: IO ()
main = do
  Sdl.withSdl "Functional Life" resolution $ flip runLife allRef

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO ()
runLife g b = do
  Sdl.clearScreen g

  alive <- Repa.selectP (Types.from . Repa.linearIndex b) (coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (rectangle pixelSize) alive

  Sdl.fillRectangles g $ Repa.toVector rectangles

  b' <- Repa.computeUnboxedP $ GoL.processLives b

  Sdl.present g

  SDL.delay 50

  runLife g b'

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !b !index = (i, j)
  where
    (Z :. !j :. !i) = Repa.fromIndex (Repa.extent b) index

{-# INLINE rectangle #-}
rectangle :: Int -> (Int, Int) -> SDL.Rectangle CInt
rectangle !ps (!x, !y) = SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (x * ps) (y * ps)) (fromIntegral <$> SDL.V2 ps ps)

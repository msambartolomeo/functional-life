{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal pattern" #-}
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
reflectorNW = (GoL.forward 19 . GoL.flipY) stopperOsci

reflectorSW :: Board
reflectorSW = (GoL.forward 4 . GoL.transpose . GoL.flipY . GoL.flipX) stopperOsci

reflectorSE :: Board
reflectorSE = GoL.forward 3 doubleOsci

reflectorNE :: Board
reflectorNE = (GoL.forward 16 . GoL.flipX . GoL.flipY) stopperOsci

reflectorPrinter :: Board
reflectorPrinter = GoL.flipX stopperOsci

bottomMachine :: Board
bottomMachine = reflectorSE <.> GoL.move (27, 45) reflectorNE <.> GoL.move (79, 57) reflectorPrinter

topMachine :: Board
topMachine = reflectorSW <.> GoL.move (28, 14) reflectorNW

resolution :: (Int, Int)
resolution = (1920, 1080)

dots :: [Bool]
-- dots = [True, False, True, False, True, True, False, True, False]
dots = take 41 $ True <$ [0 :: Int ..]

createPrinter :: [Bool] -> Board
createPrinter gs = buildGliders (topGliderPlacer l) top (zip [0 ..] first) <.> buildGliders (bottomGliderPlacer l) bottom (enumerate second)
  where
    l = length gs
    (first, second) = splitAt (l `div` 2) gs
    top = GoL.move (19 + 23 * ((l - 3) `div` 4), 0) topMachine
    bottom = GoL.move (0, 23 * ((l - 3) `div` 4) - 2) bottomMachine

topGliderPlacer :: (GameOfLife g) => Int -> Int -> g
topGliderPlacer n i = case (n `div` 2) - (i + 1) of
  0 -> GoL.move (39 + 23 * ((n - 3) `div` 4), 23) $ GoL.flipY $ GoL.fromPattern P.g3
  _ -> GoL.move (mapper ((n - 3) `div` 2) i) $ glider i
  where
    mapper l j = (26 + 12 * (j `div` 2) + 11 * ((j + 1) `div` 2), 13 + 12 * ((l - (j + 1)) `div` 2) + 11 * ((l - j) `div` 2))
    glider j = case j `mod` 2 of
      0 -> GoL.rotate $ GoL.fromPattern P.g2
      1 -> GoL.rotate $ GoL.fromPattern P.g4
      _ -> error "Unreachable"

bottomGliderPlacer :: (GameOfLife g) => Int -> Int -> g
bottomGliderPlacer n i = case (n `div` 2 + 1) - (i + 1) of
  0 -> GoL.move (20, 27 + 23 * ((n - 3) `div` 4)) . GoL.flipX $ GoL.fromPattern P.g4
  1 -> GoL.move (31, 39 + 23 * ((n - 3) `div` 4)) . GoL.flipX $ GoL.fromPattern P.g2
  _ -> GoL.move (mapper ((n - 3) `div` 2) i) $ glider i
  where
    mapper l j = (41 + 12 * ((l - j) `div` 2) + 11 * ((l - (i + 1)) `div` 2), 35 + 12 * ((j + 1) `div` 2) + 11 * (j `div` 2))
    glider j = case j `mod` 2 of
      0 -> GoL.fromPattern P.g3
      1 -> GoL.fromPattern P.g1
      _ -> error "Unreachable"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

buildGliders :: (GameOfLife g) => (Int -> g) -> g -> [(Int, Bool)] -> g
buildGliders f = foldr (\(i, b) bs -> if b then f i <.> bs else bs)

pixelSize :: Int
pixelSize = 2

main :: IO ()
main = do
  Sdl.withSdl "Functional Life" resolution (\sdl -> Sdl.mainLoop sdl runLife $ createPrinter dots)

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO Board
runLife g b = do
  alive <- Repa.selectP (Types.from . Repa.linearIndex b) (coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (rectangle pixelSize) alive

  Sdl.fillRectangles g $ Repa.toVector rectangles

  Repa.computeUnboxedP $ GoL.processLives b

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !b !index = (i, j)
  where
    (Z :. !j :. !i) = Repa.fromIndex (Repa.extent b) index

{-# INLINE rectangle #-}
rectangle :: Int -> (Int, Int) -> SDL.Rectangle CInt
rectangle !ps (!x, !y) = SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (x * ps) (y * ps)) (fromIntegral <$> SDL.V2 ps ps)

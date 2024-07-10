module Printer where

import GameOfLife (GameOfLife ((<.>)))
import GameOfLife qualified as GoL
import Patterns qualified as P

createPrinter :: (GameOfLife g) => [Bool] -> g
createPrinter gs = buildGliders (topGliderPlacer l) printerTop (enumerate top) <.> buildGliders (bottomGliderPlacer l) printerBot (enumerate bot)
  where
    enumerate = zip [0 ..]
    l = length gs
    (top, bot) = splitAt (l `div` 2) $ forceGliderCount l gs
    printerTop = GoL.move (19 + 23 * ((l - 3) `div` 4), 0) topMachine
    printerBot = GoL.move (0, 23 * ((l - 3) `div` 4) - 2) bottomMachine

-- Glider count must satisfy the condition (l `mod` 4) + 4 = 5 for the machine to work
-- If there are not enough gliders, Falses are added at the end
-- At least 6 Falses are added always as separation
forceGliderCount :: Int -> [Bool] -> [Bool]
forceGliderCount l gs = let n = l `mod` 4 + 3 in gs ++ take (12 - n) (False <$ [0 :: Int ..])

buildGliders :: (GameOfLife g) => (Int -> g) -> g -> [(Int, Bool)] -> g
buildGliders f = foldr (\(i, b) bs -> if b then f i <.> bs else bs)

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

doubleOsci :: (GameOfLife g) => g
doubleOsci = top_refector <.> GoL.move (8, 21) bottom_reflector
  where
    top_refector = GoL.fromPattern P.reflector
    bottom_reflector = GoL.flipX top_refector

stopperOsci :: (GameOfLife g) => g
stopperOsci = GoL.fromPattern P.reflector <.> GoL.move (9, 27) (GoL.fromPattern P.box)

reflectorNW :: (GameOfLife g) => g
reflectorNW = (GoL.forward 19 . GoL.flipY) stopperOsci

reflectorSW :: (GameOfLife g) => g
reflectorSW = (GoL.forward 4 . GoL.transpose . GoL.flipY . GoL.flipX) stopperOsci

reflectorSE :: (GameOfLife g) => g
reflectorSE = GoL.forward 3 doubleOsci

reflectorNE :: (GameOfLife g) => g
reflectorNE = (GoL.forward 16 . GoL.flipX . GoL.flipY) stopperOsci

reflectorPrinter :: (GameOfLife g) => g
reflectorPrinter = GoL.flipX stopperOsci

bottomMachine :: (GameOfLife g) => g
bottomMachine = reflectorSE <.> GoL.move (27, 45) reflectorNE <.> GoL.move (79, 57) reflectorPrinter

topMachine :: (GameOfLife g) => g
topMachine = reflectorSW <.> GoL.move (28, 14) reflectorNW

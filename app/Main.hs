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
import System.Exit (exitSuccess)
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

-- testGliderBot :: Board
-- testGliderBot = (GoL.move (26, 15) . GoL.flipX . GoL.flipY) $ GoL.fromPattern P.g2

-- testGliderTop :: Board
-- testGliderTop = GoL.move (22, 35) $ GoL.fromPattern P.g3

bottomMachine :: Board
bottomMachine = reflectorSE <.> GoL.move (27, 45) reflectorNE <.> GoL.move (79, 57) reflectorPrinter

topMachine :: Board
topMachine = reflectorSW <.> GoL.move (28, 14) reflectorNW

createBoard :: Board -> Board
createBoard = GoL.join (GoL.empty resolution)

resolution :: (Int, Int)
resolution = (1920, 1080)

pixelSize :: Int
pixelSize = 2

width :: Int
width = fst resolution `div` pixelSize

height :: Int
height = snd resolution `div` pixelSize

cellCount :: Int
cellCount = height * width

dots :: [Bool]
dots = take 41 $ True <$ [0 :: Int ..]

main :: IO ()
main = do
  Sdl.withSdl "Functional Life" resolution $ flip runLife $ createPrinter dots

addMiddleGlider :: Board -> Board
addMiddleGlider = GoL.join ((GoL.move (20, 23) . GoL.flipY) $ GoL.fromPattern P.g3)

addSecondLastGlider :: Board -> Board
addSecondLastGlider = GoL.join ((GoL.move (31, 41) . GoL.flipX) $ GoL.fromPattern P.g2)

addLastGlider :: Board -> Board
addLastGlider = GoL.join ((GoL.move (20, 29) . GoL.flipX) $ GoL.fromPattern P.g4)

handleEvent :: SDL.EventPayload -> IO ()
handleEvent SDL.QuitEvent = exitSuccess
handleEvent _ = return ()

createPrinter :: [Bool] -> Board
createPrinter gs = buildTopMachine l (zip [0 ..] first) <.> buildBottomMachine l (zip [0 ..] second)
  where
    l = length gs
    (first, second) = splitAt (l `div` 2) gs

size :: Int -> (Int, Int)
size l = (41 + (11 + 12) * (((l - 3) `div` 4) - 1) + 1, 13 + ((11 + 12) * ((l - 3) `div` 4)) - 15)

buildTopMachine :: Int -> [(Int, Bool)] -> Board
buildTopMachine _ [] = error "Invalid glider list"
buildTopMachine l ((_, b) : []) = GoL.expand (1000, 1000) $ GoL.move (19 + 23 * ((l - 3) `div` 4), 0) $ applyIf b addMiddleGlider topMachine
buildTopMachine l ((i, b) : bs) = applyIf b (GoL.join (GoL.move (x, y) $ glider Top i)) (buildTopMachine l bs)
  where
    startX = 26
    startY = 13
    -- (x, y) = (startX + 12 * (i `div` 2) + 11 * ((i + 1) `div` 2), startY + 12 * ((((l - 3) `div` 2) - (i + 1)) `div` 2) + 11 * ((((l - 3) `div` 2) - i) `div` 2))
    (x, y) = g startX startY l i

buildBottomMachine :: Int -> [(Int, Bool)] -> Board
buildBottomMachine _ [] = error "Invalid glider list"
buildBottomMachine l ((_, b2) : ((_, b1) : [])) = GoL.expand (1000, 1000) $ GoL.move (0, 23 * ((l - 3) `div` 4) - 2) $ applyIf b2 addSecondLastGlider $ applyIf b1 addLastGlider bottomMachine
buildBottomMachine l ((i, b) : bs) = applyIf b (GoL.join (GoL.move (x, y) $ glider Bottom i)) (buildBottomMachine l bs)
  where
    startX = 41
    startY = 35
    -- (x, y) = (startX + 12 * (i `div` 2) + 11 * ((i + 1) `div` 2), startY + 12 * ((((l - 3) `div` 2) - (i + 1)) `div` 2) + 11 * ((((l - 3) `div` 2) - i) `div` 2))
    (x, y) = f startX startY l (((l - 3) `div` 2) - 1 - i)

g :: Int -> Int -> Int -> Int -> (Int, Int)
g sx sy l i = (sx + 12 * (i `div` 2) + 11 * ((i + 1) `div` 2), sy + 12 * ((((l - 3) `div` 2) - (i + 1)) `div` 2) + 11 * ((((l - 3) `div` 2) - i) `div` 2))

f :: (Integral b) => b -> b -> b -> b -> (b, b)
f sx sy l i = (sx + 12 * ((i + 1) `div` 2) + 11 * (i `div` 2), sy + 12 * ((((l - 3) `div` 2) - i) `div` 2) + 11 * ((((l - 3) `div` 2) - (i + 1)) `div` 2))

data Side = Top | Bottom

glider :: (GameOfLife g) => Side -> Int -> g
glider Top i = case i `mod` 2 of
  0 -> GoL.rotate $ GoL.fromPattern P.g2
  1 -> GoL.rotate $ GoL.fromPattern P.g4
  _ -> error "Unreachable"
glider Bottom i = case i `mod` 2 of
  0 -> GoL.fromPattern P.g3
  1 -> GoL.fromPattern P.g1
  _ -> error "Unreachable"

applyIf :: Bool -> (a -> a) -> a -> a
applyIf False = const id
applyIf True = id

allRef :: Board
allRef = GoL.move (50, 50) $ createBoard (GoL.move (w, 0) (addMiddleGlider topMachine)) <.> GoL.move (0, h) ((addLastGlider . addSecondLastGlider) bottomMachine)
  where
    (w, h) = size $ length dots

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO ()
runLife g b = do
  SDL.pollEvents >>= foldr ((=<<) . return . handleEvent . SDL.eventPayload) (return ())

  Sdl.clearScreen g

  alive <- Repa.selectP (Types.from . Repa.linearIndex b) (coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (rectangle pixelSize) alive

  Sdl.fillRectangles g $ Repa.toVector rectangles

  b' <- Repa.computeUnboxedP $ GoL.processLives b

  Sdl.present g

  -- SDL.delay 100

  runLife g b'

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !b !index = (i, j)
  where
    (Z :. !j :. !i) = Repa.fromIndex (Repa.extent b) index

{-# INLINE rectangle #-}
rectangle :: Int -> (Int, Int) -> SDL.Rectangle CInt
rectangle !ps (!x, !y) = SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (x * ps) (y * ps)) (fromIntegral <$> SDL.V2 ps ps)

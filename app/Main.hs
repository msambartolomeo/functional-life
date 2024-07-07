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
pixelSize = 4

width :: Int
width = fst resolution `div` pixelSize

height :: Int
height = snd resolution `div` pixelSize

cellCount :: Int
cellCount = height * width

dots :: [Bool]
dots = take 9 $ True <$ [0 :: Int ..]

-- addGliders :: Board -> Board -> [Bool] -> Board
-- addGliders bot top bs = addGlidersRec bot top bs (length bs) 0

-- addGlidersRec :: Board -> Board -> [Bool] -> Int -> Int -> Board
-- addGlidersRec bot top l bs i = undefined

size :: Int -> (Int, Int)
size l = (41 + (11 + 12) * (((l - 3) `div` 4) - 1) + 1, 13 + ((11 + 12) * ((l - 3) `div` 4)) - 15)

allRef :: Board
allRef = GoL.move (50, 50) $ createBoard (GoL.move (w, 0) (addMiddleGlider topMachine)) <.> GoL.move (0, h) ((addFirstGlider . addSecondGlider) bottomMachine)
  where
    (w, h) = size $ length dots

main :: IO ()
main = do
  Sdl.withSdl "Functional Life" resolution $ flip runLife allRef

addMiddleGlider :: Board -> Board
addMiddleGlider = GoL.join ((GoL.move (20, 23) . GoL.flipY) $ GoL.fromPattern P.g3)

addFirstGlider :: Board -> Board
addFirstGlider = GoL.join ((GoL.move (31, 41) . GoL.flipX) $ GoL.fromPattern P.g2)

addSecondGlider :: Board -> Board
addSecondGlider = GoL.join ((GoL.move (20, 29) . GoL.flipX) $ GoL.fromPattern P.g4)

runLife :: Sdl.Sdl -> Array U DIM2 Life -> IO ()
runLife g b = do
  events <- SDL.pollEvents

  foldr
    ( \e m ->
        m
          >>= return
            ( case SDL.eventPayload e of
                SDL.QuitEvent -> exitSuccess
                _ -> return ()
            )
    )
    (return ())
    events

  Sdl.clearScreen g

  alive <- Repa.selectP (Types.from . Repa.linearIndex b) (coordinatesOfLinearIndex b) (Repa.size (Repa.extent b))

  rectangles <- Repa.computeP $ Repa.map (rectangle pixelSize) alive

  Sdl.fillRectangles g $ Repa.toVector rectangles

  b' <- Repa.computeUnboxedP $ GoL.processLives b

  Sdl.present g

  -- SDL.delay 5000

  runLife g b'

{-# INLINE coordinatesOfLinearIndex #-}
coordinatesOfLinearIndex :: (Source r e) => Array r DIM2 e -> Int -> (Int, Int)
coordinatesOfLinearIndex !b !index = (i, j)
  where
    (Z :. !j :. !i) = Repa.fromIndex (Repa.extent b) index

{-# INLINE rectangle #-}
rectangle :: Int -> (Int, Int) -> SDL.Rectangle CInt
rectangle !ps (!x, !y) = SDL.Rectangle (SDL.P $ fromIntegral <$> SDL.V2 (x * ps) (y * ps)) (fromIntegral <$> SDL.V2 ps ps)

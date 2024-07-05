module Patterns where

import Types (Life (..), From (from))
import Data.Array.Repa (Array, U, DIM2, Z (..), (:.) (..))
import Data.Array.Repa qualified as Repa

data Pattern = Pattern (Int, Int) [[Life]]

instance From Pattern (Array U DIM2 Life) where
  from :: Pattern -> Array U DIM2 Life
  from (Pattern (w,h) p)= Repa.fromListUnboxed (Z :. h :. w) $ concat p

glider :: Pattern
glider =
  Pattern
    (3, 3)
    [ [X, X, O],
      [O, X, O],
      [X, O, O]
    ]

reflector :: Pattern
reflector =
  Pattern
    (11, 15)
    [ [X, X, X, X, X, X, X, X, O, O, X],
      [X, X, X, X, X, X, X, X, O, O, X],
      [X, X, X, X, X, X, X, X, X, X, X],
      [X, X, X, X, X, X, X, X, X, X, X],
      [X, X, O, X, X, X, X, X, O, X, X],
      [X, O, O, O, X, X, X, O, O, O, X],
      [X, O, X, O, O, X, O, O, X, O, X],
      [O, O, X, X, X, X, X, X, X, O, O],
      [O, O, X, X, X, X, X, X, X, O, O],
      [O, O, O, X, X, X, X, X, O, O, O],
      [X, X, O, O, O, X, O, O, O, X, X],
      [X, X, X, X, O, X, O, X, X, X, X],
      [X, O, X, X, O, X, O, X, X, O, X],
      [O, X, X, O, X, X, X, O, X, X, O],
      [X, O, O, X, X, X, X, X, O, O, X]
    ]



box :: Pattern
box =
  Pattern
    (2, 2)
    [ [O, O],
      [O, O]
    ]

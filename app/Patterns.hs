module Patterns where

import Types (Life (..))

type Pattern = [[Life]]

g1 :: Pattern
g1 =
  [ [O, O, O],
    [X, X, O],
    [X, O, X]
  ]

g2 :: Pattern
g2 =
  [ [X, O, X],
    [X, O, O],
    [O, X, O]
  ]

g3 :: Pattern
g3 =
  [ [X, O, O],
    [O, X, O],
    [X, X, O]
  ]

g4 :: Pattern
g4 =
  [ [O, O, X],
    [X, O, O],
    [O, X, X]
  ]

reflector :: Pattern
reflector =
  [ [X, X, X, X, X, X, X, X, X, O, O, X, X],
    [X, X, X, X, X, X, X, X, X, O, O, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, O, X, X, X, X, X, O, X, X, X],
    [X, X, O, O, O, X, X, X, O, O, O, X, X],
    [X, X, O, X, O, O, X, O, O, X, O, X, X],
    [X, O, O, X, X, X, X, X, X, X, O, O, X],
    [X, O, O, X, X, X, X, X, X, X, O, O, X],
    [X, O, O, O, X, X, X, X, X, O, O, O, X],
    [X, X, X, O, O, O, X, O, O, O, X, X, X],
    [X, X, X, X, X, O, X, O, X, X, X, X, X],
    [X, X, O, X, X, O, X, O, X, X, O, X, X],
    [X, O, X, X, O, X, X, X, O, X, X, O, X],
    [X, X, O, O, X, X, X, X, X, O, O, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X],
    [X, X, X, X, X, X, X, X, X, X, X, X, X]
  ]

box :: Pattern
box =
  [ [O, O],
    [O, O]
  ]

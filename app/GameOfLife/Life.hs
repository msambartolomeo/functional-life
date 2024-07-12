module GameOfLife.Life where

import Data.Vector.Unboxed.Deriving (derivingUnbox)

-- Define my own Coerce class as Data.Coercible can not be instanciated manually
class Coerce a b where
  coerce :: a -> b

data Life = O | X deriving (Eq, Show)

instance Coerce Life Int where
  {-# INLINE coerce #-}
  coerce :: Life -> Int
  coerce O = 1
  coerce X = 0

instance Coerce Life Bool where
  {-# INLINE coerce #-}
  coerce :: Life -> Bool
  coerce O = True
  coerce X = False

instance Coerce Bool Life where
  {-# INLINE coerce #-}
  coerce :: Bool -> Life
  coerce True = O
  coerce False = X

-- Make Life work in Repa Unboxed arrays as if it was a Bool
derivingUnbox
  "Life"
  [t|Life -> Bool|]
  [|coerce|]
  [|coerce|]

{-# INLINE liveOrDie #-}
liveOrDie :: Life -> Int -> Life
liveOrDie _ 3 = O
liveOrDie O 2 = O
liveOrDie _ _ = X

{-# INLINE joinLife #-}
joinLife :: Life -> Life -> Life
joinLife O _ = O
joinLife X l = l
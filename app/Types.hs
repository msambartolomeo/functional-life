module Types where

import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word8)
import SDL (V4 (..))

-- Define my own Coerce class as Data.Coercible can not be instanciated manually
class Coerce a b where
  coerce :: a -> b

data Color = White | Black

instance Coerce Color (SDL.V4 Word8) where
  {-# INLINE coerce #-}
  coerce :: Color -> SDL.V4 Word8
  --                  R   G   B   a
  coerce White = SDL.V4 255 255 255 255
  coerce Black = SDL.V4 0 0 0 255

data Life = O | X deriving (Eq, Show)

{-# INLINE joinLife #-}
joinLife :: Life -> Life -> Life
joinLife O _ = O
joinLife X l = l

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

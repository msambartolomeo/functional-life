module Types where

import Data.Vector.Unboxed.Deriving (derivingUnbox)
import Data.Word (Word8)
import SDL (V4 (..))

class From a b where
  from :: a -> b

data Color = White | Black

instance From Color (SDL.V4 Word8) where
  {-# INLINE from #-}
  from :: Color -> SDL.V4 Word8
  --                  R   G   B   a
  from White = SDL.V4 255 255 255 255
  from Black = SDL.V4 0 0 0 255

data Life = O | X deriving (Eq, Show)

joinLife :: Life -> Life -> Life
joinLife O _ = O
joinLife X l = l

instance From Life Int where
  {-# INLINE from #-}
  from :: Life -> Int
  from O = 1
  from X = 0

instance From Life Bool where
  {-# INLINE from #-}
  from :: Life -> Bool
  from O = True
  from X = False

instance From Bool Life where
  {-# INLINE from #-}
  from :: Bool -> Life
  from True = O
  from False = X

derivingUnbox
  "Life"
  [t|Life -> Bool|]
  [|from|]
  [|from|]

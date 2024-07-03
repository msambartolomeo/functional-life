module Types where

import Data.Word (Word8)
import SDL (V4 (..))

class From a b where
  from :: a -> b

instance From Bool Int where
  {-# INLINE from #-}
  from :: Bool -> Int
  from True  = 1
  from False = 0

data Color = White | Black

instance From Color (SDL.V4 Word8) where
  {-# INLINE from #-}
  from :: Color -> SDL.V4 Word8
  --                  R   G   B   a
  from White = SDL.V4 255 255 255 255
  from Black = SDL.V4 0   0   0   255

-- data Life = Alive | Dead
type Life = Bool

module Format where

import Data.Word
import Color
import Matrix
import Int2

-- utilities

encodeABGR :: Int -> Int -> Matrix RGBN -> [Word8]
encodeABGR w h x = concatMap f [(i,j) | j <- [0..h-1], i <- [0..w-1]] where
  conv = floor . (255 *)
  f (i,j) = case Matrix.lookup x (I2 i j) of
    Invis           -> [0,0,0,0]
    Vis (RGB r g b) -> [255, conv b, conv g, conv r]

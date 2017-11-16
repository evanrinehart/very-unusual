module Image where

import Data.Monoid
import Data.Maybe
import Control.Applicative

import Color
import R2

type Image a = R2 -> a
type Picture = Image RGBN

blank xy = Invis
triangle p1 p2 p3 c1 c2 xy = if inside (p1,p2,p3) xy then c1 else c2
shift xy pic = pic . (xy ^-^)
scale s pic = pic . (^/ s)
rotate theta pic = pic . (rotR2 (-theta))

rectangle rx ry w h c (R2 x y)
  | x < rx || x > rx + w = Nothing
  | y < ry || y > ry + h = Nothing
  | otherwise = Just c

layer :: Monoid a => Image a -> Image a -> Image a
layer = mappend

t1 = triangle p1 p2 p3 red black
t2 = shift (R2 (-1) 0) (triangle p1 p2 p3 blue black)
p1 = R2 2 2
p2 = R2 (-2) 1
p3 = R2 2 (-2)

rasterize :: Image a -> R2 -> Int -> Int -> [[a]]
rasterize pic origin wi hi = answer where
  w = fromIntegral wi
  h = fromIntegral hi
  answer = map f [0..h-1]
  f y = map (g y) [0..w-1]
  g y x = pic (origin ^+^ R2 (x+0.5) (y+0.5))

module Color where

data RGB = RGB !Double !Double !Double
  deriving (Show, Eq, Ord)

data RGBN = Invis | Vis RGB deriving (Show, Eq, Ord)

instance Monoid RGBN where
  mempty = Invis
  mappend (Vis c) _ = Vis c
  mappend Invis x   = x

color8 r g b = RGB
  (fromIntegral r / 255.0)
  (fromIntegral g / 255.0) 
  (fromIntegral b / 255.0)

white = color8 255 255 255
black = color8 0 0 0
red = color8 255 0 0
green = color8 0 255 0
blue = color8 0 0 255
yellow = color8 255 255 0
magenta = color8 255 0 255
cyan = color8 0 255 255
gray = color8 128 128 128

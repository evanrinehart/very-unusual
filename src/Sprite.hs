module Sprite where

import Table
import Matrix
import Color
import Image

import Data.Word

data Sprite a = Sprite
  { sprW     :: !Int
  , sprH     :: !Int
  , sprOX    :: !Int
  , sprOY    :: !Int
  , sprPix   :: a }

type ContinuousSprite = Sprite Picture
type RasterSprite     = Sprite (Matrix RGBN)
type CachedSprite     = Sprite Int

data SpriteSheet = SpriteSheet
  { anim1 :: Table CachedSprite
  , anim2 :: Table CachedSprite
  , anim3 :: Matrix CachedSprite }

h = 255
m = 128

dat :: [Word8]
dat = concat
 [[h,0,0,0],[h,0,0,m],[h,0,0,h],[h,0,0,h]
 ,[h,0,m,0],[h,0,m,0],[h,0,m,0],[h,0,m,0]
 ,[h,m,0,0],[h,m,0,m],[h,m,0,h],[h,m,0,h]
 ,[h,m,m,m],[h,m,0,m],[h,0,0,m],[h,m,0,0]]

errorSprite :: Sprite Picture
errorSprite = Sprite
  { sprW = 16
  , sprH = 16
  , sprOX = 8
  , sprOY = 8
  , sprPix = blank }



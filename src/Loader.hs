module Loader where

import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Video
import Sprite
import Matrix
import Table
import Format
import Image
import Int2
import R2
import Color

newSpriteKey :: Monad m => StateT Int m Int
newSpriteKey = do
  i <- get
  put (i + 1)
  return i

cacheSprite :: Sprite Picture -> Int -> IO (Sprite Int)
cacheSprite spr@(Sprite w h _ _ pic) i = do
  let (w',h') = (fromIntegral w, fromIntegral h)
  let bottomLeft = R2 (-w'/2) (-h'/2)
  let raster = Matrix.fromList Invis (rasterize pic bottomLeft w h)
  load (I2 w h) (encodeABGR w h raster) i
  return (spr { sprPix = i })

cacheSprite' :: Sprite Picture -> StateT Int IO (Sprite Int)
cacheSprite' spr = do
  i <- newSpriteKey
  liftIO (cacheSprite spr i)

a1sprs = undefined
a2sprs = undefined
a3sprs = undefined

loadStaticImages :: StateT Int IO SpriteSheet
loadStaticImages = do
  err <- cacheSprite' errorSprite
  a1 <- Table.fromList err <$> mapM cacheSprite' a1sprs
  a2 <- Table.fromList err <$> mapM cacheSprite' a2sprs
  a3 <- Matrix.fromList err <$> mapM (mapM cacheSprite') a3sprs
  return (SpriteSheet a1 a2 a3)


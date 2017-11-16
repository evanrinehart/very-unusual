module Main where

import Control.Concurrent
import Control.Monad
import Data.Word

import Int2
import Video
import Sprite
import Loader
import R2
import Color
import Image

deg d = d*pi / 180

testImage (R2 x y)
  | even (floor (y / 8)) = Vis yellow
  | otherwise = Vis black
testSprite = Sprite 64 64 32 32 (rotate (deg 45) testImage)
--testSprite = Sprite 64 64 32 32 testImage

main :: IO ()
main = do
  initialize 1
  cacheSprite testSprite 0
  forever $ do
    x <- getEvent
    case x of
      Just y -> do
        print y
      Nothing -> return ()
    clear
    paste 0 (I2 0 0)
    present

-- independent behaviors of the world
-- 1. internally sequenced events
-- 2. audio generation
-- 3. external events occur
-- 4. vsync samples the world

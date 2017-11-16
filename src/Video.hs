module Video where

import qualified Graphics.UI.SDL as SDL
import Control.Exception
import Data.Typeable
import Data.Vector.Storable (Vector)
import System.IO.Unsafe
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IORef
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal
import Data.IVar.Simple (IVar)
import qualified Data.IVar.Simple as IVar
import Control.Monad
import qualified System.Clock as Clock

import Int2
import Dir

data RawIn = Joy1 Dir PR | Joy2 Dir PR | P1 | P2 | J1 PR | J2 PR | Coin
  deriving Show

type VCache = IntMap (Int,Int,SDL.Texture)

-- globals
cacheRef :: IORef VCache
{-# NOINLINE cacheRef #-}
cacheRef = unsafePerformIO (newIORef IM.empty)

rendererIVar :: IVar SDL.Renderer
{-# NOINLINE rendererIVar #-}
rendererIVar = unsafePerformIO IVar.new

renderer :: SDL.Renderer
renderer = IVar.read rendererIVar

windowIVar :: IVar SDL.Window
{-# NOINLINE windowIVar #-}
windowIVar = unsafePerformIO IVar.new

window :: SDL.Window
window = IVar.read windowIVar

hScaleIVar :: IVar Int
{-# NOINLINE hScaleIVar #-}
hScaleIVar = unsafePerformIO IVar.new

hScale :: Int
hScale = IVar.read hScaleIVar


-- methods
initialize :: Int -> IO ()
initialize hScale = do
  IVar.write hScaleIVar hScale
  r <- SDL.init SDL.SDL_INIT_EVERYTHING
  when (r < 0) throwSDL
  (win,rend) <- alloca $ \winref -> do
    alloca $ \rendref -> do
      let flags = 0
      r <- SDL.createWindowAndRenderer (320 * fromIntegral hScale) 240 flags winref rendref
      when (r < 0) throwSDL
      win <- peek winref
      rend <- peek rendref
      return (win,rend)
  IVar.write windowIVar win
  IVar.write rendererIVar rend

shutdown :: IO ()
shutdown = do
  SDL.quit

clear :: IO ()
clear = do
  SDL.setRenderDrawColor renderer 0 0 0 255
  SDL.renderClear renderer
  return ()

paste :: Int -> Int2 -> IO ()
paste i (I2 x y) = do
  cache <- readIORef cacheRef
  case IM.lookup i cache of
    Nothing -> throwIO (SDLException ("paste " ++ show (i, I2 x y) ++ " failed, (not found)"))
    Just (w,h,tex) -> alloca $ \dest -> do
      let fi = fromIntegral
      poke dest (SDL.Rect (fi (x * hScale)) (fi y) (fi (w * hScale)) (fi h))
      r <- SDL.renderCopy renderer tex nullPtr dest
      when (r < 0) throwSDL

present :: IO ()
present = do
  SDL.renderPresent renderer

getEvent :: IO (Maybe RawIn)
getEvent = answer where
  just = return . Just
  nada = return Nothing :: IO (Maybe RawIn)
  pr SDL.SDL_PRESSED = Press
  pr SDL.SDL_RELEASED = Release
  answer = alloca $ \evref -> do
    r <- SDL.pollEvent evref
    if r == 0 then nada else do
      ev <- peek evref
      case ev of
        SDL.QuitEvent _ _ -> exitSDL
        SDL.KeyboardEvent _ _ _ state rep (SDL.Keysym _ key _) ->
          if rep == 1
            then nada
            else case key of
              SDL.SDLK_w -> just $ Joy1 North (pr state)
              SDL.SDLK_a -> just $ Joy1 West (pr state)
              SDL.SDLK_s -> just $ Joy1 South (pr state)
              SDL.SDLK_d -> just $ Joy1 East (pr state)
              SDL.SDLK_e -> just $ J1 (pr state)
              SDL.SDLK_1 -> if state == SDL.SDL_PRESSED then just P1 else nada
              SDL.SDLK_2 -> if state == SDL.SDL_PRESSED then just P2 else nada
              SDL.SDLK_4 -> if state == SDL.SDL_PRESSED then just Coin else nada
              SDL.SDLK_ESCAPE -> exitSDL
              _ -> nada
        _ -> nada
              
    
load :: Int2 -> [Word8] -> Int -> IO ()
load (I2 w h) pixels i = do
  existing <- fmap (IM.lookup i) (readIORef cacheRef)
  case existing of
    Just (_,_,tex) -> SDL.destroyTexture tex
    Nothing -> return ()
  let fi = fromIntegral
  tex <- SDL.createTexture renderer SDL.SDL_PIXELFORMAT_RGBA8888 SDL.SDL_TEXTUREACCESS_STATIC (fi w) (fi h)
  when (tex == nullPtr) throwSDL
  withArray pixels $ \arrayRef -> do
    r <- SDL.updateTexture tex nullPtr (castPtr arrayRef) (fi (w * 4))
    when (r < 0) throwSDL
  modifyIORef' cacheRef (IM.insert i (w,h,tex))

getNanos :: IO Integer
getNanos = do
  ts <- Clock.getTime Clock.Monotonic
  return (Clock.toNanoSecs ts)

--- exception

data SDLException = SDLException String
  deriving (Typeable)

instance Show SDLException where
  show (SDLException msg) = msg

instance Exception SDLException

throwSDL :: IO a
throwSDL = SDL.getError >>= peekCAString >>= throwIO . SDLException

exitSDL :: IO a
exitSDL = do
  shutdown
  throwIO (SDLException "got SDL_QUIT event")
  

--type AudioCb = Ptr () -> Ptr Word8 -> Int -> IO ()
--
---foreign import ccall "wrapper"
--  mkAudioCb :: AudioCb -> IO (FunPtr AudioCb)

--audioCbWrapper :: (s -> Int -> IO (Vector Double)) -> s -> AudioCb
--audioCbWrapper moreSamples appState uptr outbuf bufSize = do
  -- loop through the 


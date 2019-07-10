{-# LANGUAGE OverloadedStrings #-}
module Animation
    ( ex01
    , ex02
    , ex03
    , someFunc
    ) where

import Control.Monad
import Control.Concurrent (threadDelay)
import Foreign.C.Types
import SDL.Vect
import SDL (($=))
import qualified SDL
import qualified SDL.Image
import Paths_animation (getDataFileName)

ex01 :: IO ()
ex01 = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Animation" SDL.defaultWindow { SDL.windowInitialSize = V2 640 480 }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window

  dinopath <- getDataFileName "resources/dino.png"
  dinoimage <- SDL.Image.load dinopath

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        SDL.surfaceBlit dinoimage Nothing screenSurface Nothing
        SDL.updateWindowSurface window

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.freeSurface dinoimage
  SDL.quit

ex02 :: IO ()
ex02 = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Animation" SDL.defaultWindow { SDL.windowInitialSize = V2 800 600 }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1) SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedRenderer, SDL.rendererTargetTexture = False}

  SDL.rendererDrawColor renderer $= V4 minBound maxBound minBound maxBound

  dinopath <- getDataFileName "resources/tileset.png"
  dinoimage <- SDL.Image.load dinopath
  dinotexture <- SDL.createTextureFromSurface renderer dinoimage
  SDL.freeSurface dinoimage

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        SDL.clear renderer

        SDL.copy renderer dinotexture Nothing Nothing

        SDL.present renderer

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyTexture dinotexture
  SDL.destroyWindow window
  SDL.quit


ex03 :: IO ()
ex03 = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow "Animation" SDL.defaultWindow { SDL.windowInitialSize = V2 800 600 }
  SDL.showWindow window

  renderer <- SDL.createRenderer window (-1) SDL.RendererConfig { SDL.rendererType = SDL.AcceleratedRenderer, SDL.rendererTargetTexture = False}

  SDL.rendererDrawColor renderer $= V4 minBound maxBound minBound maxBound

  dinopath <- getDataFileName "resources/Gladiator-Sprite Sheet.png"
  dinoimage <- SDL.Image.load dinopath
  dinotexture <- SDL.createTextureFromSurface renderer dinoimage
  SDL.freeSurface dinoimage

  let pos = SDL.Rectangle (P (V2 5 5)) (V2 20 20)

  let spriteSize = V2 100 100
      clip1 = SDL.Rectangle (P (V2 0 0)) spriteSize
      clip2 = SDL.Rectangle (P (V2 100 0)) spriteSize
      clip3 = SDL.Rectangle (P (V2 0 100)) spriteSize
      clip4 = SDL.Rectangle (P (V2 100 100)) spriteSize

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        SDL.clear renderer

        SDL.copy renderer dinotexture (Just pos) (Just pos)

        SDL.present renderer

        unless quit loop

  loop

  SDL.destroyRenderer renderer
  SDL.destroyTexture dinotexture
  SDL.destroyWindow window
  SDL.quit


someFunc :: IO ()
someFunc = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "Animation" SDL.defaultWindow { SDL.windowInitialSize = V2 640 480 }
  SDL.showWindow window

  screenSurface <- SDL.getWindowSurface window

  dinopath <- getDataFileName "resources/dino.png"
  dinoimage <- SDL.Image.load dinopath

  tilesetpath <- getDataFileName "resources/tileset.png"
  tilesetimage <- SDL.Image.load tilesetpath

  let loop = do
        events <- SDL.pollEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        SDL.surfaceBlit tilesetimage Nothing screenSurface Nothing
        SDL.updateWindowSurface window

        unless quit loop

  loop

  -- SDL.surfaceBlit dinoimage Nothing screenSurface Nothing
  -- -- SDL.surfaceFillRect screenSurface Nothing white
  -- SDL.updateWindowSurface window

  -- threadDelay 2000000

  SDL.destroyWindow window
  SDL.freeSurface dinoimage
  SDL.quit

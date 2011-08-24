module Bindings (
  display,reshape,keyboardMouse
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Display
import TetrisWorld
import Block

reshape :: Size -> IO()
reshape s@(Size w h) = do
  viewport $= (Position 0 0, s)

keyboardMouse :: IORef World -> Key -> KeyState -> t -> t1 -> IO()
keyboardMouse world key state modifiers position = do
  w <- get world
  world $= worldAfterKey w key state

worldAfterKey :: World -> Key -> KeyState -> World
worldAfterKey world k Down
  | k == (Char 'q')            = error "quit!"
  | k == (Char ' ')            = fastFall world
  | k == (SpecialKey KeyLeft)  = moveLeft world
  | k == (SpecialKey KeyRight) = moveRight world
  | k == (SpecialKey KeyUp)    = rotateTetra world CounterClockwise
  | k == (SpecialKey KeyDown)  = moveDown world
worldAfterKey world _ _ = world

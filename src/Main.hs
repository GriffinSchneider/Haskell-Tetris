module Main where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Bindings
import Piece
import Block
import TetrisWorld
import System.Random
import Array

main :: IO()
main = do
  (progname,_) <- getArgsAndInitialize
--  initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
  initialDisplayMode $= [DoubleBuffered]
  createWindow "Hello World"
  windowSize $= Size 400 400
  initGL

  let foo g = inspect_stream $ randomElts g [0..6]
  seed <- newStdGen
  world <- newIORef $ makeGameWithPieceList (foo seed)

  animate world
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (keyboardMouse world)
  displayCallback $= disp world
  mainLoop

-- Helper functions to generate a random list
-- taken from http://www.haskell.org/pipermail/haskell-cafe/2007-July/029783.html
randomElts rg xs = map (arr !) (randomRs bds rg)
  where bds = (1, length xs)
        arr = listArray bds xs
inspect_stream = foldr (\x -> (seq x).(x:)) []

initGL :: IO ()
initGL = do
 depthFunc $= Just Gequal
 scale 0.1 (-0.1) (0.1::GLfloat)

disp :: IORef World -> IO()
disp world = do
  w <- get world
  display w
  addTimerCallback 1 $ disp world

animate :: IORef World -> IO()
animate world = do
  w <- get world
  print w
  world $= timerTick w
  addTimerCallback 500 $ animate world

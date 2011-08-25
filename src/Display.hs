module Display (
  display,
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef

import Cube
import Block
import Piece
import TetrisWorld

drawCube :: GLfloat -> Vector3 GLfloat -> Color4 GLfloat -> IO()
drawCube w v c = preservingMatrix $ do
  translate v
  color c
  cube w
  color $ Color4 0 0 0 (1.0::GLfloat)
  cubeLines w

drawBlock :: Block -> IO ()
drawBlock (Block (Vector2 x y) c) = drawCube 0.5 (Vector3 x y 0.0) c

drawPiece :: Piece -> IO ()
drawPiece (Piece blocks _) = foldl (\ acc block -> (drawBlock block) >> acc) (return()) blocks

drawStack :: Stack -> IO ()
drawStack s =  foldl (\ acc block -> (drawBlock block) >> acc) (return()) (getAllBlocks s)

drawGhost :: World -> IO ()
drawGhost w@(World p s _) = drawPiece $ changePieceColor (fastFallPiece w) (Color4 1.0 1.0 1.0 0.3)

drawWorld :: World -> IO ()
drawWorld w@(World p s _) = drawGhost w >> drawPiece p >> drawStack s

display world = do
  clear [ColorBuffer, DepthBuffer]
--  rotate 0.05 (Vector3 0 1.0 (0::GLfloat))
  drawWorld world
  swapBuffers

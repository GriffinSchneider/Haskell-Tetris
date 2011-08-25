module Block (
  Block(Block),
  RotateDirection(Clockwise, CounterClockwise),
  shiftBlocks,
  matchesVector,
  rotateBlock,
  isBlockOutsideBounds,
  left,
  right,
  down,
) where

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Cube

data Block = Block (Vector2 GLfloat) (Color4 GLfloat)

instance Show Block where show (Block (Vector2 x y) _) = "|" ++ (show x) ++ ", " ++ (show (round y)) ++ "|"

data RotateDirection = Clockwise | CounterClockwise

left  = (Vector2 (-1) (0::GLfloat))
right = (Vector2   1  (0::GLfloat))
down  = (Vector2   0  (1::GLfloat))

shiftBlocks :: [Block] -> Vector2 GLfloat -> [Block]
shiftBlocks blocks v = map (\ b -> shiftBlock b v) blocks

rotateBlock :: Block -> Vector2 GLfloat -> RotateDirection -> Block
rotateBlock (Block v color) center direction = Block (rotateVector v center direction) color

rotateVector :: Vector2 GLfloat -> Vector2 GLfloat -> RotateDirection -> Vector2 GLfloat
rotateVector (Vector2 x y) (Vector2 cx cy) CounterClockwise = Vector2 (cx + (cy - y)) (cy + (x - cx))
rotateVector v c Clockwise =
  rotateVector (rotateVector (rotateVector v c CounterClockwise) c CounterClockwise) c CounterClockwise

matchesVector :: Block -> Vector2 GLfloat -> Bool
matchesVector (Block (Vector2 x y) _) (Vector2 x1 y1) = x == x1 && y == y1

isBlockOutsideBounds :: Block -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Bool
isBlockOutsideBounds (Block (Vector2 x y) _) minX minY maxX maxY = x < minX || x > maxX || y < minY || y > maxY

-- Helper functions
shiftBlock :: Block -> Vector2 GLfloat -> Block
shiftBlock (Block (Vector2 x y) c) (Vector2 tx ty) = (Block (Vector2 (x + tx) (y + ty)) c)

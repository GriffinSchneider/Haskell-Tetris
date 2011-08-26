module Block (
  Block(Block),
  RotateDirection(Cw, Ccw),
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

instance Show Block where
  show (Block (Vector2 x y) _) = "|" ++ (show x) ++ ", " ++ (show (round y)) ++ "|"

-- | RotateDirection - Clockwise or Counter-Clockwise
data RotateDirection = Cw | Ccw

left  = (Vector2 (-1) (0::GLfloat))
right = (Vector2   1  (0::GLfloat))
down  = (Vector2   0  (1::GLfloat))

shiftBlocks :: [Block] -> Vector2 GLfloat -> [Block]
shiftBlocks blocks v = map (\ b -> shiftBlock b v) blocks
 where shiftBlock (Block (Vector2 x y) c) (Vector2 tx ty) = (Block (Vector2 (x + tx) (y + ty)) c)

rotateBlock :: Block -> Vector2 GLfloat -> RotateDirection -> Block
rotateBlock (Block v color) center direction = Block (rotateVector v center direction) color

rotateVector :: Vector2 GLfloat -> Vector2 GLfloat -> RotateDirection -> Vector2 GLfloat
rotateVector v@(Vector2 x y) c@(Vector2 cx cy) dir = case dir of
  Ccw -> Vector2 (cx + (cy - y)) (cy + (x - cx))
  _  -> rotateVector (rotateVector (rotateVector v c Ccw) c Ccw) c Ccw

matchesVector :: Block -> Vector2 GLfloat -> Bool
matchesVector (Block (Vector2 x y) _) (Vector2 x1 y1) = x == x1 && y == y1

isBlockOutsideBounds :: Block -> Vector4 GLfloat -> Bool
isBlockOutsideBounds (Block (Vector2 x y) _) (Vector4 minX minY maxX maxY) =
   x < minX || x > maxX || y < minY || y > maxY

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
  showsBlock
) where

import Graphics.UI.GLUT
import Cube
import Constants

data Block = Block V2 (Color4 GLfloat)

instance Show Block where show b = showsBlock b ""
showsBlock :: Block -> ShowS
showsBlock (Block (V2 x y) _) = ('|':) . shows x . (", "++) . shows (round y) . ('|':)

-- | RotateDirection - Clockwise or Counter-Clockwise
data RotateDirection = Cw | Ccw

left  = (V2 (-1) 0)
right = (V2   1  0)
down  = (V2   0  1)

shiftBlocks :: [Block] -> V2 -> [Block]
shiftBlocks blocks v = map (\ b -> shiftBlock b v) blocks
 where shiftBlock (Block v c) tv = Block (translateV2 v tv) c

rotateBlock :: Block -> V2 -> RotateDirection -> Block
rotateBlock (Block v color) center direction = Block (rotateVector v center direction) color

rotateVector :: V2 -> V2 -> RotateDirection -> V2
rotateVector v@(V2 x y) c@(V2 cx cy) dir = case dir of
  Ccw -> V2 (cx + (cy - y)) (cy + (x - cx))
  _   -> rotateVector (rotateVector (rotateVector v c Ccw) c Ccw) c Ccw

matchesVector :: Block -> V2 -> Bool
matchesVector (Block (V2 x y) _) (V2 x1 y1) = x == x1 && y == y1

isBlockOutsideBounds :: Block -> V4 -> Bool
isBlockOutsideBounds (Block (V2 x y) _) (V4 minX minY maxX maxY) =
   x < minX || x > maxX || y < minY || y > maxY

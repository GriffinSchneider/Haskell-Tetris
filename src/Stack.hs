module Stack (
  Stack,
  addPiece,
  removeFullRows,
  showStack,
) where

import Graphics.Rendering.OpenGL
import Block
import Piece
import Data.List

type Stack = [[Block]]

showStack s = showsStack s ""

showsStack :: Stack -> ShowS
showsStack = foldr (\bs acc -> ("    "++) . shows bs . ('\n':) . acc) (""++)

addPiece :: Piece -> Stack -> Stack
addPiece (Piece blocks _) s = foldr (\b acc -> addBlock b acc) s blocks
  where addBlock b@(Block (Vector2 _ y) _) s = addBlockAtY b (round y) s

removeFullRows :: Stack -> Int -> Stack
removeFullRows s width = fixBlockYs $ removeRows s $ getFullRowIndices width s
  where getFullRowIndices width s = findIndices (isRowFull width) s
        isRowFull width blocks    = (length blocks) >= width

-- Helper functions
addBlockAtY :: Block -> Int -> Stack -> Stack
addBlockAtY b r ss = help b (9 - r) ss
  where help b r []     = if (r == 0) then [[b]]    else []:(help b (r - 1) [])
        help b r (s:ss) = if (r == 0) then (b:s):ss else  s:(help b (r - 1) ss)

removeRows :: Stack -> [Int] -> Stack
removeRows stack nums = [x | (x,n) <- zip stack [0..], not $ elem n nums]

-- Set block Y values to match the row they're in.
fixBlockYs :: Stack -> Stack
fixBlockYs stack = [setBlockYs n bs | (bs,n) <- zip stack [0..]]
  where setBlockYs y blocks = map (\ (Block (Vector2 x _) c) -> (Block (Vector2 x (9-y)) c)) blocks

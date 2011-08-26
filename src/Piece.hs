module Piece (
  PieceShape(O,I,T,J,L,Z,S),
  Piece(Piece),
  movePiece,
  containsBlock,
  makePiece,
  rotatePiece,
  isPieceOutsideBounds,
  changePieceColor,
) where

import Graphics.Rendering.OpenGL hiding (T, S)
import Block
import Constants

data PieceShape = O | I | T | J | L | Z | S
   deriving (Show)

data Piece = Piece [Block] V2
    deriving (Show)

makePiece :: PieceShape -> Piece
makePiece shape = Piece (shiftBlocks (blocksFromShape shape) center) center
  where center  = startingCoordsFromShape shape
        blocksFromShape s = map (\ v -> Block v (colorFromShape s)) (blockCoordsFromShape s)

movePiece :: Piece -> V2 -> Piece
movePiece (Piece blocks v) v1 = Piece (shiftBlocks blocks v1) (translateVector v v1)
  where translateVector (V2 x1 y1) (V2 x2 y2) = V2 (x1 + x2) (y1 + y2)

rotatePiece :: Piece -> RotateDirection -> Piece
rotatePiece (Piece blocks v) d = Piece (rotateBlocks blocks d v) v
 where rotateBlocks blocks d v = map (\ b -> rotateBlock b v d) blocks

containsBlock :: Piece -> Block -> Bool
containsBlock (Piece blocks _) (Block v _) = any (\ block -> (matchesVector block v)) blocks

isPieceOutsideBounds :: Piece -> V4 -> Bool
isPieceOutsideBounds (Piece blocks _) bounds = any (\ b -> isBlockOutsideBounds b bounds) blocks

changePieceColor :: Piece -> Color4 GLfloat -> Piece
changePieceColor (Piece blocks v) color = (Piece (map (\ (Block v _) -> (Block v color)) blocks) v)

-- Helper functions
startingCoordsFromShape :: PieceShape -> V2
startingCoordsFromShape shape = case shape of
  O -> V2 0.5 0.5
  _ -> V2 0   0

blockCoordsFromShape :: PieceShape -> [V2]
blockCoordsFromShape O =
  [V2 (-0.5) (-0.5),
   V2   0.5  (-0.5),
   V2 (-0.5)   0.5 ,
   V2   0.5    0.5]
blockCoordsFromShape I =
  [V2   0.0  (-1.0),
   V2   0.0    0.0 ,
   V2   0.0    1.0 ,
   V2   0.0    2.0]
blockCoordsFromShape T =
  [V2   0.0    0.0 ,
   V2 (-1.0) (-1.0),
   V2   0.0  (-1.0),
   V2   1.0  (-1.0)]
blockCoordsFromShape J =
  [V2   0.0  (-1.0),
   V2   0.0    0.0 ,
   V2   0.0    1.0 ,
   V2 (-1.0)   1.0]
blockCoordsFromShape L =
  [V2   0.0  (-1.0),
   V2   0.0    0.0 ,
   V2   0.0    1.0 ,
   V2   1.0    1.0]
blockCoordsFromShape Z =
  [V2   0.0    0.0,
   V2   1.0    0.0,
   V2   1.0    1.0,
   V2   2.0    1.0]
blockCoordsFromShape S =
  [V2   1.0    0.0 ,
   V2   2.0    0.0 ,
   V2   0.0    1.0 ,
   V2   1.0    1.0]

colorFromShape :: PieceShape -> Color4 GLfloat
colorFromShape O = Color4 1.0 1.0 1.0 1.0
colorFromShape I = Color4 1.0 1.0 0.0 1.0
colorFromShape T = Color4 1.0 0.0 1.0 1.0
colorFromShape J = Color4 0.0 1.0 1.0 1.0
colorFromShape L = Color4 1.0 0.0 0.0 1.0
colorFromShape Z = Color4 0.0 1.0 0.0 1.0
colorFromShape S = Color4 0.0 0.0 1.0 1.0

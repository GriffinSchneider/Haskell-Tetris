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

data PieceShape = O | I | T | J | L | Z | S
   deriving (Show)

data Piece = Piece [Block] (Vector2 GLfloat)
    deriving (Show)

makePiece :: PieceShape -> Piece
makePiece shape = Piece (shiftBlocks (blocksFromShape shape) center) center
  where center  = startingCoordsFromShape shape
        blocksFromShape s = map (\ v -> Block v (colorFromShape s)) (blockCoordsFromShape s)

movePiece :: Piece -> Vector2(GLfloat) -> Piece
movePiece (Piece blocks v) v1 = Piece (shiftBlocks blocks v1) (translateVector v v1)
  where translateVector (Vector2 x1 y1) (Vector2 x2 y2) = Vector2 (x1 + x2) (y1 + y2)

rotatePiece :: Piece -> RotateDirection -> Piece
rotatePiece (Piece blocks v) d = Piece (rotateBlocks blocks d v) v
 where rotateBlocks blocks d v = map (\ b -> rotateBlock b v d) blocks

containsBlock :: Piece -> Block -> Bool
containsBlock (Piece blocks _) (Block v _) = any (\ block -> (matchesVector block v)) blocks

isPieceOutsideBounds :: Piece -> Vector4 GLfloat -> Bool
isPieceOutsideBounds (Piece blocks _) bounds = any (\ b -> isBlockOutsideBounds b bounds) blocks

changePieceColor :: Piece -> Color4 GLfloat -> Piece
changePieceColor (Piece blocks v) color = (Piece (map (\ (Block v _) -> (Block v color)) blocks) v)

-- Helper functions
startingCoordsFromShape :: PieceShape -> Vector2 GLfloat
startingCoordsFromShape shape = case shape of
  O -> Vector2 0.5 0.5
  _ -> Vector2 0   0

blockCoordsFromShape :: PieceShape -> [Vector2 GLfloat]
blockCoordsFromShape O =
  [Vector2 (-0.5) (-0.5),
   Vector2   0.5  (-0.5),
   Vector2 (-0.5)   0.5 ,
   Vector2   0.5    0.5]
blockCoordsFromShape I =
  [Vector2   0  (-1),
   Vector2   0    0 ,
   Vector2   0    1 ,
   Vector2   0    2]
blockCoordsFromShape T =
  [Vector2   0    0 ,
   Vector2 (-1) (-1),
   Vector2   0  (-1),
   Vector2   1  (-1)]
blockCoordsFromShape J =
  [Vector2   0  (-1),
   Vector2   0    0 ,
   Vector2   0    1 ,
   Vector2 (-1)   1]
blockCoordsFromShape L =
  [Vector2   0  (-1),
   Vector2   0    0 ,
   Vector2   0    1 ,
   Vector2   1    1]
blockCoordsFromShape Z =
  [Vector2   0    0 ,
   Vector2   1    0 ,
   Vector2   1    1 ,
   Vector2   2    1]
blockCoordsFromShape S =
  [Vector2   1    0 ,
   Vector2   2    0 ,
   Vector2   0    1 ,
   Vector2   1    1]

colorFromShape :: PieceShape -> Color4 GLfloat
colorFromShape O = Color4 1.0 1.0 1.0 1.0
colorFromShape I = Color4 1.0 1.0 0.0 1.0
colorFromShape T = Color4 1.0 0.0 1.0 1.0
colorFromShape J = Color4 0.0 1.0 1.0 1.0
colorFromShape L = Color4 1.0 0.0 0.0 1.0
colorFromShape Z = Color4 0.0 1.0 0.0 1.0
colorFromShape S = Color4 0.0 0.0 1.0 1.0

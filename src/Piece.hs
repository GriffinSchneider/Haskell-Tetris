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

import Graphics.Rendering.OpenGL
import ListComprehension
import Block

data PieceShape = O | I | T | J | L | Z | S
    deriving (Show)

data Piece = Piece [Block] (Vector2 GLfloat)
    deriving (Show)

makePiece :: PieceShape -> Piece
makePiece shape = Piece (shiftBlocks (blocksFromShape shape) center) center
  where center  = startingCoordsFromShape shape
        blocksFromShape s = map (\ v -> Block v (colorFromShape s)) (blockCoordsFromShape s)
rotateBlocks :: [Block] -> RotateDirection -> Vector2(GLfloat) -> [Block]
rotateBlocks blocks d v = map (\ b -> rotateBlock b v d) blocks

movePiece :: Piece -> Vector2(GLfloat) -> Piece
movePiece (Piece blocks v) v1@(Vector2 tx ty) = Piece (shiftBlocks blocks v1) (translateVector v tx ty)
  where translateVector (Vector2 x1 y1) x2 y2 = Vector2 (x1 + x2) (y1 + y2)

rotatePiece :: Piece -> RotateDirection -> Piece
rotatePiece (Piece blocks v) d = Piece (rotateBlocks blocks d v) v

containsBlock :: Piece -> Block -> Bool
containsBlock (Piece blocks _) (Block v _) = ormap (\ block -> (matchesVector block v)) blocks

isPieceOutsideBounds :: Piece -> GLfloat -> GLfloat -> GLfloat -> GLfloat -> Bool
isPieceOutsideBounds (Piece blocks _) minX minY maxX maxY = ormap (\ b -> isBlockOutsideBounds b minX minY maxX maxY) blocks

changePieceColor :: Piece -> Color4 GLfloat -> Piece
changePieceColor (Piece blocks v) color = (Piece (map (\ (Block v _) -> (Block v color)) blocks) v)

-- Helper functions
startingCoordsFromShape :: PieceShape -> Vector2 GLfloat
startingCoordsFromShape Piece.O = Vector2 0.5 0.5
startingCoordsFromShape _ = Vector2 0 0

blockCoordsFromShape :: PieceShape -> [Vector2 GLfloat]
blockCoordsFromShape Piece.O =
  [Vector2 (-0.5) (-0.5),
   Vector2   0.5  (-0.5),
   Vector2 (-0.5)   0.5 ,
   Vector2   0.5    0.5]
blockCoordsFromShape Piece.I =
  [Vector2   0  (-1),
   Vector2   0    0 ,
   Vector2   0    1 ,
   Vector2   0    2]
blockCoordsFromShape Piece.T =
  [Vector2   0    0 ,
   Vector2 (-1) (-1),
   Vector2   0  (-1),
   Vector2   1  (-1)]
blockCoordsFromShape Piece.J =
  [Vector2   0  (-1),
   Vector2   0    0 ,
   Vector2   0    1 ,
   Vector2 (-1)   1]
blockCoordsFromShape Piece.L =
  [Vector2   0  (-1),
   Vector2   0    0 ,
   Vector2   0    1 ,
   Vector2   1    1]
blockCoordsFromShape Piece.Z =
  [Vector2   0    0 ,
   Vector2   1    0 ,
   Vector2   1    1 ,
   Vector2   2    1]
blockCoordsFromShape Piece.S =
  [Vector2   1    0 ,
   Vector2   2    0 ,
   Vector2   0    1 ,
   Vector2   1    1]

colorFromShape :: PieceShape -> Color4 GLfloat
colorFromShape Piece.O = Color4 1.0 1.0 1.0 1.0
colorFromShape Piece.I = Color4 1.0 1.0 0.0 1.0
colorFromShape Piece.T = Color4 1.0 0.0 1.0 1.0
colorFromShape Piece.J = Color4 0.0 1.0 1.0 1.0
colorFromShape Piece.L = Color4 1.0 0.0 0.0 1.0
colorFromShape Piece.Z = Color4 0.0 1.0 0.0 1.0
colorFromShape Piece.S = Color4 0.0 0.0 1.0 1.0

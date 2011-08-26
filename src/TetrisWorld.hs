module TetrisWorld (
  World(World),
  Stack,
  timerTick,
  moveLeft,
  moveRight,
  moveDown,
  rotateTetra,
  fastFall,
  fastFallPiece,
  makeGameWithPieceList,
  kWorldBounds,
) where

import Graphics.Rendering.OpenGL hiding (T, S)
import Piece
import Block
import System.Random
import Array
import Stack

data World = World Piece Stack [Int]

instance Show World where
  show (World p s _) = "World: \n  Piece: \n    " ++ (show p) ++ "\n  Stack: \n" ++ (showStack s)

kWorldBounds = Vector4 (-4) (-100) 5 9

timerTick :: World -> World
timerTick world = moveDown world

makeGameWithPieceList :: [Int] -> World
makeGameWithPieceList nums = getNextPiece $ World (makePiece T) [] nums

-- Key event functions
moveLeft :: World -> World
moveLeft world@(World p s nums) =
  let newWorld = World (movePiece p left) s nums in
    if (isCollision newWorld) then world else newWorld

moveRight :: World -> World
moveRight world@(World p s nums) =
  let newWorld = World (movePiece p right) s nums in
    if (isCollision newWorld) then world else newWorld

moveDown :: World -> World
moveDown world@(World p s nums) =
  let newWorld = World (movePiece p down) s nums in
    if (isCollision newWorld) then combinePiece world else newWorld

rotateTetra :: World -> RotateDirection -> World
rotateTetra world@(World p s nums) d =
  let newWorld = World (rotatePiece p d) s nums in
    if (isCollision newWorld) then world else newWorld

fastFall :: World -> World
fastFall world@(World _ s nums) = combinePiece (World (fastFallPiece world) s nums)

fastFallPiece :: World-> Piece
fastFallPiece world@(World p s nums) =
  let oneDown = World (movePiece p down) s nums in
    if (isCollision oneDown) then p else fastFallPiece oneDown

-- Helper functions
getNextPiece :: World -> World
getNextPiece (World _ stack nums) = World (pieceFromNumber (head nums)) stack (tail nums)
  where pieceFromNumber n = (map makePiece [O,I,T,J,L,Z,S]) !! n

combinePiece :: World -> World
combinePiece (World p s nums) = getNextPiece $ World p (removeFullRows (addPiece p s) 10) nums

isCollision :: World -> Bool
isCollision w@(World p s _) = (any (containsBlock p) (concat s)) || (isOutsideBounds w)
  where isOutsideBounds (World p _ _) = isPieceOutsideBounds p kWorldBounds

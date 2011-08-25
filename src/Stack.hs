module Stack (
  Stack,
  getAllBlocks,
  addPiece,
  removeFullRows,
  showStack,
) where

import Graphics.Rendering.OpenGL
import Block
import Piece

type Stack = [[Block]]

showStack :: Stack -> String
showStack (bs:bss) = "    " ++ (show bs) ++ "\n" ++ (showStack bss)
showStack [] = ""

getAllBlocks :: Stack -> [Block]
getAllBlocks stack = foldr (\x acc -> addAllToList x acc) [] stack
  where addAllToList l1 l2 = foldr (\x acc -> x:acc) l2 l1

addPiece :: Piece -> Stack -> Stack
addPiece (Piece blocks _) s = addBlocks blocks s
  where addBlock b@(Block (Vector2 _ y) _) s = addBlockAtRow b (round y) s
        addBlocks bs s = foldr (\b acc -> addBlock b acc) s bs

removeFullRows :: Stack -> Int -> Stack
removeFullRows s width = setBlockYsByRowNumber $ removeRows s fullRows
  where fullRows = getFullRowIndices s width
        getFullRowIndices s width = getTrueIndices $ map (\b -> isRowFull b width) s
        isRowFull blocks width = (length blocks) >= width

-- Helper functions

addBlockAtRow :: Block -> Int -> Stack -> Stack
addBlockAtRow b r ss = addBlockAtRowH b (9 - r) ss
addBlockAtRowH b r   []   = if (r == 0) then [[b]]    else []:(addBlockAtRowH b (r - 1) [])
addBlockAtRowH b r (s:ss) = if (r == 0) then (b:s):ss else  s:(addBlockAtRowH b (r - 1) ss)

removeRows :: Stack -> [Int] -> Stack
removeRows stack nums = removeRowsH stack nums 0
removeRowsH :: Stack -> [Int] -> Int -> Stack
removeRowsH (s:[]) nums acc = if elem acc nums then [] else [s]
removeRowsH (s:ss) nums acc = if elem acc nums then x else s:x
  where x = removeRowsH ss nums (acc + 1)

getTrueIndices :: [Bool] -> [Int]
getTrueIndices l =  getTrueIndicesH l 0
getTrueIndicesH :: [Bool] -> Int -> [Int]
getTrueIndicesH [] acc = []
getTrueIndicesH (b:bs) acc = if b then acc:next else next
  where next = getTrueIndicesH bs (acc + 1)

setBlockYsByRowNumber :: Stack -> Stack
setBlockYsByRowNumber stack = setBlockYsByRowNumberH stack 0
setBlockYsByRowNumberH :: Stack -> Int -> Stack
setBlockYsByRowNumberH [] acc = []
setBlockYsByRowNumberH (bs:bss) acc = (map (\ (Block (Vector2 x _) c) -> (Block (Vector2 x (fromIntegral (9 - acc))) c)) bs):(setBlockYsByRowNumberH bss (acc + 1))

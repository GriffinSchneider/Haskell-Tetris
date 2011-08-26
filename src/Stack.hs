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
  where fullRows                  = getFullRowIndices s width
        getFullRowIndices s width = getTrueIndices $ map (\b -> isRowFull b width) s
        isRowFull blocks width    = (length blocks) >= width

-- Helper functions
addBlockAtRow :: Block -> Int -> Stack -> Stack
addBlockAtRow b r ss = help b (9 - r) ss
  where help b r   []   = if (r == 0) then [[b]]    else []:(help b (r - 1) [])
        help b r (s:ss) = if (r == 0) then (b:s):ss else  s:(help b (r - 1) ss)

removeRows :: Stack -> [Int] -> Stack
removeRows stack nums = help stack nums 0
 where help (s:[]) nums acc = if elem acc nums then [] else [s]
       help (s:ss) nums acc = if elem acc nums then x else s:x
         where x = help ss nums (acc + 1)

getTrueIndices :: [Bool] -> [Int]
getTrueIndices l =  help l 0
  where help [] acc = []
        help (b:bs) acc = if b then acc:next else next
          where next = help bs (acc + 1)

setBlockYsByRowNumber :: Stack -> Stack
setBlockYsByRowNumber stack = help stack 0
 where help [] acc = []
       help (bs:bss) acc =
         (:) (map (\ (Block (Vector2 x _) c) -> (Block (Vector2 x (fromIntegral (9 - acc))) c))
                  bs)
             (help bss (acc + 1))

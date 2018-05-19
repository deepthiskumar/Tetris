module Piece where

import Prelude hiding (drop, take, reverse)
import Types
import Data.Sequence
import Data.List ((\\))
--This module lists all the pieces and 
--the operations that can be performed on a piece

--Pieces I | J | L | O | S | T | Z | E
--1. I
initPos = ((div gridWidth 2)-2, 0)

pieceI :: Piece
pieceI = Piece 
  (fromList [fromList[I,E,E,E],fromList[I,E,E,E],fromList[I,E,E,E],fromList[I,E,E,E]])
  initPos
  
pieceJ :: Piece
pieceJ = Piece 
  (fromList [fromList[E,J,E],fromList[E,J,E],fromList[J,J,E]])
  initPos
  
pieceL :: Piece
pieceL = Piece 
  (fromList [fromList[L,E,E],fromList[L,E,E],fromList[L,L,E]])
  initPos

pieceO :: Piece
pieceO = Piece 
  (fromList [fromList[O,O,E],fromList[O,O,E],fromList[E,E,E]])
  initPos
  
pieceS :: Piece
pieceS = Piece 
  (fromList [fromList[E,S,S],fromList[S,S,E],fromList[E,E,E]])
  initPos

pieceT :: Piece
pieceT = Piece 
  (fromList [fromList[T,T,T],fromList[E,T,E],fromList[E,E,E]])
  initPos
  
pieceZ :: Piece
pieceZ = Piece 
  (fromList [fromList[Z,Z,E],fromList[E,Z,Z],fromList[E,E,E]])
  initPos
  
--Rotate once clockwise
rotate :: Piece -> Piece
rotate i@(Piece s p) 
  | s == (struct pieceO) = i 
  (Piece (rotateSeq s) p) 

rotateTwice = rotate.rotate

rotateThrice = rotate.rotate.rotate

--PROPERTY
-- (rotate $ rotateThrice s) == s

--move operations

moveLeft :: Piece -> Piece
moveLeft p@(Piece s (x,y))   
  | isValid (x,y) = Piece s (x-1,y)
  | otherwise     = p

moveRight p@(Piece s (x,y))   
  | isValid (x,y) = Piece s (x+1,y)
  | otherwise     = p

moveDown p@(Piece s (x,y)) 
  | isValid (x,y) = Piece s (x,y+1)
  | otherwise     = p
  
isValid :: (Int,Int) -> Bool
isValid (x,y)
  | x >= 0 && y >= 0 && x < gridWidth && y < gridHeight = True
  | otherwise = False 

newPiece :: Int -> Piece
newPiece = index pieces

------------------------------------------------
--helpers
------------------------------------------------
flatSeq :: Seq (Seq a) -> Seq a
flatSeq xs = foldr (><) empty xs

rotateSeq :: Seq (Seq a) -> Seq (Seq a)
rotateSeq (Empty :<| xs) = empty
rotateSeq s     = let s' = reverse s
  in (flatSeq $ fmap (take 1) s') <| (rotateSeq $ reverse $ fmap (drop 1) s')
  
pieces :: Seq Piece
pieces = let l = [pieceI, pieceJ, pieceL,pieceS, pieceT, pieceZ]
  in fromList $ l ++ (map rotate l) 
    ++ (map rotateTwice (l\\[pieceI, pieceZ, pieceS])) ++ (map rotateThrice (l\\[pieceI, pieceZ, pieceS]))
      ++ [pieceO]


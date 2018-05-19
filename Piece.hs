module Piece where

import Prelude hiding (drop, take, reverse)
import Types
import Data.Sequence --(Seq (Empty, (:<|), (:|>)), drop, take, fromList, reverse, (><), mapWithIndex, (<|), empty )
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
  (fromList [fromList[E,J,E,E],fromList[E,J,E,E],fromList[J,J,E,E],fromList[E,E,E,E]])
  initPos
  
pieceL :: Piece
pieceL = Piece 
  (fromList [fromList[L,E,E,E],fromList[L,E,E,E],fromList[L,L,E,E],fromList[E,E,E,E]])
  initPos

pieceO :: Piece
pieceO = Piece 
  (fromList [fromList[O,O,E,E],fromList[O,O,E,E],fromList[E,E,E,E],fromList[E,E,E,E]])
  initPos
  
pieceS :: Piece
pieceS = Piece 
  (fromList [fromList[E,S,S,E],fromList[S,S,E,E],fromList[E,E,E,E],fromList[E,E,E,E]])
  initPos

pieceT :: Piece
pieceT = Piece 
  (fromList [fromList[T,T,T,E],fromList[E,T,E,E],fromList[E,E,E,E],fromList[E,E,E,E]])
  initPos
  
pieceZ :: Piece
pieceZ = Piece 
  (fromList [fromList[Z,Z,E,E],fromList[E,Z,Z,E],fromList[E,E,E,E],fromList[E,E,E,E]])
  initPos
  
--Rotate once clockwise
rotate :: Piece -> Piece
rotate (Piece s p) = (Piece (rotateSeq s) p) 

rotateTwice = rotate.rotate

rotateThrice = rotate.rotate.rotate

--PROPERTY
-- (rotate $ rotateThrice s) == s

--move operations

moveLeft :: Piece -> Piece
moveLeft (Piece s (x,y)) = Piece s (x-1,y)

moveRight (Piece s (x,y)) = Piece s (x+1,y)

moveDown (Piece s (x,y)) = Piece s (x,y+1)

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
  in (flatSeq $ fmap (take 1) s') <| (rotateSeq $ reverse $ mapWithIndex (\i x -> drop 1 x) s')
  
pieces :: Seq Piece
pieces = let l = [pieceI, pieceJ, pieceL, pieceO, pieceS, pieceT, pieceZ]
  in fromList $ l ++ (map rotate l) ++ (map rotateTwice l) ++ (map rotateThrice l)


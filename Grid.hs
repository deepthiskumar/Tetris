module Grid where

import Prelude hiding (length, replicate, drop)
import Types
import Piece
import Data.Sequence

--Operations that can be performed on the layout
-- and on the peice wrt the layout.

--1. construct the layout
newGrid :: Int -> Int -> Grid
newGrid h w = replicate h (replicate w E)

--2. Remove rows that do not have any empty cell
--TODO check only the lines that coincide with the current piece's position
removeFilledRows :: Grid -> Grid
removeFilledRows gr = let (full, notFull) = partition isFull gr
  in padEmptyRows (length full) notFull

padEmptyRows :: Int -> Grid -> Grid
padEmptyRows n gr = (newGrid n gridWidth) >< gr


isFull :: Seq Cell -> Bool
isFull Empty       = True
isFull (E :<| xs ) = False
isFull (_ :<| xs ) = isFull xs


--3. check if the piece location is valid

isValidPos :: Piece -> Grid -> Bool
isValidPos p@(Piece s (x,y)) gr = noOverlap p gr
  
noOverlap :: Piece -> Grid -> Bool
noOverlap (Piece s (x,y)) gr = 
  and [i >= 0 && j >= 0 && i < gridWidth && j < gridHeight && index (index gr j) i == E | i <- [x..x+3], j <- [y..y+3], 
    index (index s (j-y)) (i-x) /= E   ]

--4. update grid with the new piece
updateGrid :: Piece -> Grid -> Grid
updateGrid (Piece Empty _) gr = gr
updateGrid (Piece s (x,y)) gr = let gr' = update y (updateRow x (index s 0) (index gr y)) gr
  in updateGrid (Piece (drop 1 s) (x,y+1)) gr'


updateRow :: Int -> Seq Cell -> Seq Cell -> Seq Cell
updateRow _ Empty gRow = gRow
updateRow x pRow gRow = let gRow' = update x (index pRow 0) gRow
  in updateRow (x+1) (drop 1 pRow) gRow'
 













 
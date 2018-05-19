module Types where

import Data.Sequence
import Control.Monad.Trans.State.Strict

--A piece in the game of tetris can be drawn on a 2D structure of the order 4x4
--A piece on the layout will consist of a position denoted by a x and y corodinates.

data Piece = Piece {
  struct :: Seq (Seq Cell),
  pos :: (Int,Int)
  }
  deriving(Show, Eq)

--A cell can contain either of the following enum values. E refers to an empty cell
data Cell = I | J | L | O | S | T | Z | E
  deriving(Show, Eq)

--A grid is again a 2D structure similar to a piece but of order height x width

type Grid = Seq (Seq Cell)

-- Height and Width of the game layout
gridHeight = 20 :: Int
gridWidth  = 20 :: Int

--User operations
--Move left, right, down and rotate the piece

data Move = Le | Ri | Do | Ro

--Different stages of the game

data GameStage = NewPiece
  | PieceFalling
  | GameOver
  deriving(Show)

--Game State

data World = World {
  grid :: Grid,
  currentPiece :: Piece,
  score :: Int,
  gameStage :: GameStage,
  timeCounter :: Int }
  deriving(Show)
  
type GameState a = StateT World IO a

--Game Speed (multiple levels can be introduced but for now it's just one speed)
speed = 15 :: Int




 



module GameEngine where

import Grid
import Types
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import GameIO
import Piece
import Debug.Trace


start :: IO ()
start = evalStateT play initWorld

--the game loop
play :: GameState ()
play = printWorld >> step >> next

next :: GameState ()
next = do
  world <- get
  case gameStage world of
    GameOver -> printWorld >> return ()
    otherwise -> play

step :: GameState ()
step = do
  w@(World g p s gs) <- get
  case gs of
    GameOver     -> return ()
    NewPiece     -> do
      let newPiece = pieceT--TODO randPiece
      case isValidPos newPiece g of
        True  -> put (World g newPiece s PieceFalling)
        False -> put (World g p s GameOver)
    PieceFalling -> do
      liftIO getUserMove >>= completeUserMove
      --lock the piece? 

completeUserMove :: Maybe Move -> GameState ()
completeUserMove move = do
  w@(World g p s _) <- get
  case move of
        Just Ro ->  do
                     if isValidPos (rotate p) g then
                       put (World g (rotate p) s PieceFalling)
                     else put w
        Just Do ->  do
                     if isValidPos (moveDown p) g then 
                       put (World g (moveDown p) s PieceFalling)
                     else put w
        Just Le ->  do
                     if isValidPos (moveLeft p) g then 
                       put (World g (moveLeft p) s PieceFalling)
                     else put w
        Just Ri ->  do
                     if isValidPos (moveRight p) g then 
                       put (World g (moveRight p) s PieceFalling)
                     else put w
        Nothing ->  put w

initState :: GameState ()
initState = put initWorld

initWorld :: World
initWorld = World (newGrid gridHeight gridWidth) newPiece 0 PieceFalling


newPiece :: Piece
newPiece = pieceJ --undefined --randomly select from the 28 pieces

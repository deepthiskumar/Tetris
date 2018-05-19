module GameEngine where

import Grid
import Types
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import System.IO
import GameIO
import Piece
import Control.Concurrent


start :: IO ()
start = (hSetEcho stdin False) >> randPiece >>= e
  where e p = evalStateT play (initWorld p) 

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
  w@(World g p s gs t) <- get
  case gs of
    GameOver     -> return ()
    NewPiece     -> do
      newPiece <- liftIO randPiece--TODO randPiece
      case isValidPos newPiece g of
        True  -> put (World g newPiece s PieceFalling (t+1))
        False -> put (World g p s GameOver (t+1))
    PieceFalling -> do
      --timed fall
      liftIO $ (threadDelay (100000))
      if(t == speed) && (isValidPos (moveDown p) g) then
        put (World g (moveDown p) s PieceFalling 0)
      else --put (World g p s gs (t+1))
        liftIO getUserMove >>= completeUserMove
      --lock the piece
      w'@(World g' p' s' gs' t') <- get
      case isValidPos (moveDown p') g' of
        True  -> return ()
        False -> do
          let (n, g'') = removeFilledRows (updateGrid p' g')
          put (World g'' p' (s'+(n*10)) NewPiece 0)

completeUserMove :: Maybe Move -> GameState ()
completeUserMove move = do
  w@(World g p s gs t) <- get
  case move of
        Just Ro ->  do
                     if isValidPos (rotate p) g then
                       put (World g (rotate p) s PieceFalling (t+1))
                     else put (World g p s gs (t+1))
        Just Do ->  do
                     if isValidPos (moveDown p) g then 
                       put (World g (moveDown p) s PieceFalling (t+1))
                     else put (World g p s gs (t+1))
        Just Le ->  do
                     if isValidPos (moveLeft p) g then 
                       put (World g (moveLeft p) s PieceFalling (t+1))
                     else put (World g p s gs (t+1))
        Just Ri ->  do
                     if isValidPos (moveRight p) g then 
                       put (World g (moveRight p) s PieceFalling (t+1))
                     else put (World g p s gs (t+1))
        Nothing ->  put (World g p s gs (t+1))


initWorld :: Piece -> World
initWorld p = World (newGrid gridHeight gridWidth) p 0 PieceFalling 0

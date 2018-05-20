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
start = (hSetBuffering stdin NoBuffering) >> 
  (hSetEcho stdin False) >> randPiece >>= e
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
      newPiece <- liftIO randPiece
      case isValidPos newPiece g of
        True  -> put (World g newPiece s PieceFalling (t+1))
        False -> put (World g newPiece s GameOver (t+1))
    PieceFalling -> do
      liftIO $ (threadDelay (100000))
      --timed fall
      if(t == speed) && (isValidPos (moveDown p) g) then
        put (World g (moveDown p) s PieceFalling 0)
      else
        liftIO getUserMove >>= completeUserMove >> lockPiece


lockPiece :: GameState ()
lockPiece = do
  w'@(World g' p' s' gs' t') <- get
  case isValidPos (moveDown p') g' of
    True  -> return ()
    False -> do
      let (n, g'') = removeFilledRows (updateGrid p' g')
      put (World g'' p' (s'+(n*points)) NewPiece 0)

completeUserMove :: Maybe Move -> GameState ()
completeUserMove move = do
  w@(World g p s gs t) <- get
  case move of
        Just Ro ->  put $ userMove rotate w
        Just Do ->  put $ userMove moveDown w
        Just Le ->  put $ userMove moveLeft w
        Just Ri ->  put $ userMove moveRight w
        Nothing ->  put (World g p s gs (t+1))

userMove :: (Piece -> Piece) -> World -> World
userMove f (World g p s gs t)
  | isValidPos (f p) g = (World g (f p) s PieceFalling (t+1))
  | otherwise          = (World g p s gs (t+1))

initWorld :: Piece -> World
initWorld p = World (newGrid gridHeight gridWidth) p 0 PieceFalling 0

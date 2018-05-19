module GameIO where

import Types
import Data.Sequence hiding (replicate)
import Grid
import Piece
import System.Console.ANSI
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Concurrent.Async
import System.Random

import System.IO
import System.Console.Haskeline

printWorld :: GameState()
printWorld = do
  world <- get
  liftIO $ doIOStuff world

doIOStuff :: World -> IO ()
doIOStuff w = do
  --hSetBuffering stdin NoBuffering
  clearScreen >> setCursorPosition 0 0
  putStrLn $ showWorld w


showWorld :: World -> String
showWorld w = let g' = updateGrid (currentPiece w) (grid w)
  in (concat $ fmap showRow g') ++ (replicate (gridWidth+2) '@') ++ "\nScore: " ++ show (score w)
  
showRow :: Seq Cell -> String
showRow r = "@" ++ (concat $ fmap showCell r) ++ "@\n"

showCell E = " "
showCell x = show x
  
--data Move = Le | Ri | Do | Ro
getUserMove :: IO (Maybe Move)
getUserMove = hReady stdin >>= f
  where
  f True = do 
   minput <- getChar
   case minput of
    'k' -> return (Just Ro)
    'j' -> return (Just Do)
    'h' -> return (Just Le)
    'l' -> return (Just Ri)
    otherwise -> return Nothing
  f False = return Nothing

randPiece :: IO Piece
randPiece = getStdRandom (randomR (0,18)) >>= return.newPiece 

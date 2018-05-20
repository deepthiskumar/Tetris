module GameIO where

import Types
import qualified Data.Sequence as S hiding (replicate)
import Grid
import Piece
import System.Console.ANSI
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Control.Concurrent.Async
import System.Random
import System.IO

border = "*"

printWorld :: GameState()
printWorld = do
  world <- get
  liftIO $ doIOStuff world

doIOStuff :: World -> IO ()
doIOStuff w = do
  clearScreen >> setCursorPosition 0 0
  putStrLn $ showWorld w


showWorld :: World -> String
showWorld w = let g' = updateGrid (currentPiece w) (grid w)
  in (concat $ fmap showRow g')
    ++ (concat $ replicate (gridWidth+2) (border++" "))
      ++ "\nScore: " ++ show (score w)
  
showRow :: S.Seq Cell -> String
showRow r = border ++ " " ++ (concat $ fmap showCell r) ++ border ++ "\n"

showCell E = "  "
showCell x = "O "
  

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
randPiece = getStdRandom (randomR (0,S.length pieces -1))
  >>= return.newPiece

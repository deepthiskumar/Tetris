module GameIO where

import Types
import Data.Sequence hiding (replicate)
import Grid
import System.Console.ANSI
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict

import System.IO
import System.Console.Haskeline


import Control.Monad
import UI.NCurses

nc :: IO ()
nc = runCurses $ do
    w <- newWindow 20 15 0 0
    setEcho False
    forever $ do
        e <- getEvent w Nothing
        updateWindow w $ do
            moveCursor 0 0
            case e of
              Just (EventSpecialKey KeyUpArrow)    -> drawString ("UpArrow")
              Just (EventSpecialKey KeyDownArrow)  -> drawString ("DownArrow")
              Just (EventSpecialKey KeyRightArrow) -> drawString ("RightArrow")
              Just (EventSpecialKey KeyLeftArrow)  -> drawString ("LeftArrow")
              otherwise                            -> return ()
        render



{- data World = World {
  grid :: Grid,
  currentPiece :: Piece,
  score :: Int,
  gameStage :: GameStage }
--}

printWorld :: GameState()
printWorld = do
  world <- get
  liftIO $ doIOStuff world

doIOStuff :: World -> IO ()
doIOStuff w@(World g p s gs ) = do
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
getUserMove = do
  ---hSetEcho stdin False
  minput <- getChar
  case minput of
    'k' -> return (Just Ro) --putStrLn $ "\nInput was: up arrow"
    'j' -> return (Just Do) --putStrLn $ "\nInput was: down arrow"
    'h' -> return (Just Le) --putStrLn $ "\nInput was: left arrow"
    'l' -> return (Just Ri) -- putStrLn $ "\nInput was: right arrow"
    otherwise -> return Nothing





test :: IO ()
test = loop
   where
       loop :: IO ()
       loop = do
           hSetEcho stdin False
           minput <- getChar
           case minput of
             'k' -> putStrLn $ "\nInput was: up arrow"
             'j' -> putStrLn $ "\nInput was: down arrow"
             'h' -> putStrLn $ "\nInput was: left arrow"
             'l' -> putStrLn $ "\nInput was: right arrow"
             otherwise -> putStrLn $ "\n Input was: " ++ show minput
           loop


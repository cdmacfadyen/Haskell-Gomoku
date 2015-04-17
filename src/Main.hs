module Main where

import System.Environment
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Board
import Draw
import Input
import AI

-- | Initialize the main IO loop.
main :: IO ()		-- ^ IO State.
-- | Starts up a graphics window and sets up handlers for 
-- dealing with inputs and updating the world state.
main = do args <- getArgs
          background <- loadBMP "images/background.bmp"
          black_piece <- loadBMP "images/black.bmp"
          white_piece <- loadBMP "images/white.bmp"
          undo_btn <- loadBMP "images/undo.bmp"
          save_btn <- loadBMP "images/save.bmp"
          undo_btn_h <- loadBMP "images/undo-highlight.bmp"
          save_btn_h <- loadBMP "images/save-highlight.bmp"
          world <- if length args > 0
                      then loadGame $ args !! 0
                      else return $ initWorld 600
          -- Keeping draw and update world 'pure' functions which are then converted to IO equivalents only in this function, 
          -- since they don't do any IO actions themselves.
          playIO (InWindow "Gomoku" (1100, 1100) (10, 10)) (greyN 0.3) 10
                 world
                 --(\x -> drawWorld x background)
                 (return . (\x -> drawWorld x background black_piece white_piece undo_btn save_btn undo_btn_h save_btn_h)) -- Convert the world state to gloss state.
                 -- | Called if there is an input event. If it is the
           	      --human player's turn, should update the board.
                 handleInput -- handleInput is an impure function since it saves/loads files.
                 -- | Called 10 times per second. If it is AI's turn,
                 -- should update the board with an AI generated move.
                 (curry $ return . (uncurry updateWorld))

loadGame :: String -> IO World
loadGame filename = do putStrLn $ "Loading game from file: " ++ show filename
                       contents <- readFile filename
                       return (read contents :: World)
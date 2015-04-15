module Main where

import System.Environment
import Graphics.Gloss.Interface.IO.Game
import Board
import Draw
import Input
import AI

-- | Initialize the main IO loop.
main :: IO ()		-- ^ IO State.
-- | Starts up a graphics window and sets up handlers for 
-- dealing with inputs and updating the world state.
main = do args <- getArgs
          world <- if length args > 0
              then loadGame $ args !! 0
              else return $ initWorld 600
          -- Keeping draw and update world 'pure' functions which are then converted to IO equivalents only in this function, since they don't do any IO actions themselves.
          playIO (InWindow "Gomoku" (640, 640) (10, 10)) aquamarine 10
                 world
                 (return . drawWorld) -- Convert the world state to gloss state.
                 -- | Called if there is an input event. If it is the
           	     -- human player's turn, should update the board.
                 handleInput -- handleInput is an impure function since it saves/loads files.
                 -- | Called 10 times per second. If it is AI's turn,
                 -- should update the board with an AI generated move.
                 (curry $ return . (uncurry updateWorld))

loadGame :: String -> IO World
loadGame filename = do putStrLn $ "Loading game from file: " ++ show filename
                       contents <- readFile filename
                       return (read contents :: World)

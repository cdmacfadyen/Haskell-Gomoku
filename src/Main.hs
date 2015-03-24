module Main where

import Graphics.Gloss
import Board
import Draw
import Input
import AI

-- | Initialize the main IO loop.
main :: IO ()		-- ^ IO State.
-- | Starts up a graphics window and sets up handlers for 
-- dealing with inputs and updating the world state.
main = play (InWindow "Gomoku" (640, 640) (10, 10)) aquamarine 10
            (initWorld 600)
            drawWorld -- Convert the world state to gloss state.
            -- | Called if there is an input event. If it is the
           	-- human player's turn, should update the board.
            handleInput
            -- | Called 10 times per second. If it is AI's turn,
            -- should update the board with an AI generated move.
            updateWorld

            


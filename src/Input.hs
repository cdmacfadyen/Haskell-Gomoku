module Input(handleInput) where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Board
import AI
import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
--
-- trace :: String -> a -> a
-- 'trace' returns its second argument while printing its first argument
-- to stderr, which can be a very useful way of debugging!
handleInput :: Event -> World -> World
handleInput (EventMotion (x, y)) b 
    = trace ("Mouse moved to: " ++ show (x,y)) b

-- Initiates check to see if placement is valid
-- Passes in point that's closest to a valid placement coord
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b = case maybepos of
    Just pos  -> maybeBoardToWorld b $ makeMove (board b) (turn b) pos
    Nothing   -> b
    where maybepos = screenSpaceToBoardSpace b (x, y)

handleInput (EventKey (Char k) Down _ _) b
    = trace ("Key " ++ show k ++ " down") b
handleInput (EventKey (Char k) Up _ _) b
    = trace ("Key " ++ show k ++ " up") b
handleInput e b = b

-- Gets a World from original World and Maybe Board
-- Have to check if Maybe Board is Nothing or Board before returning World
maybeBoardToWorld :: World -> Maybe Board -> World
maybeBoardToWorld b Nothing = b
maybeBoardToWorld b (Just mBoard) = b {board = mBoard, turn = switch (turn b)}


{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

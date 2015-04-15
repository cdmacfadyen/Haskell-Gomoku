module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
import Board
import AI
import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
handleInput :: Event -> World -> IO World
handleInput (EventMotion (x, y)) b = return b {mousePos = pos}
    where pos = screenSpaceToBoardSpace b (x, y)

-- Initiates check to see if placement is valid
-- Passes in point that's closest to a valid placement coord
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) b = case maybepos of
    Just pos  -> return $ maybeBoardToWorld b $ makeMove (board b) (turn b) pos
    Nothing   -> return b
    where maybepos = mousePos b

handleInput (EventKey (Char 's') Down _ _) b = do saveGame "gomoku.save" b
                                                  return b
handleInput (EventKey (Char k) Down _ _) b = return $ trace ("Is there a win? " ++ show (checkWon b)) b
handleInput (EventKey (Char k) Up _ _) b = return b
handleInput e b = return b

-- Gets a World from original World and Maybe Board
-- Have to check if Maybe Board is Nothing or Board before returning World
maybeBoardToWorld :: World -> Maybe Board -> World
maybeBoardToWorld b Nothing = b
maybeBoardToWorld b (Just mBoard) = b {board = mBoard, turn = switch (turn b)}

saveGame :: String -> World -> IO ()
saveGame filename w = do putStrLn $ "Game saved to file: " ++ show filename
                         writeFile filename (show w)

{- Hint: when the 'World' is in a state where it is the human player's
 turn to move, a mouse press event should calculate which board position
 a click refers to, and update the board accordingly.

 At first, it is reasonable to assume that both players are human players.
-}

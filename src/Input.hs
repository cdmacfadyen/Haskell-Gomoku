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

handleInput (EventKey (Char 'u') Down _ _) b = return $ undo b
handleInput (EventKey (Char 's') Down _ _) b = do saveGame "gomoku.save" b
                                                  return b
handleInput (EventKey (Char k) Down _ _) b = return $ trace ("Is there a win? " ++ show (checkWon b)) b
handleInput (EventKey (Char k) Up _ _) b = return b
handleInput e b = return b

maybeBoardToWorld :: World -> Maybe Board -> World
maybeBoardToWorld b Nothing = b
maybeBoardToWorld b (Just mBoard) = b {board = mBoard, turn = switch (turn b)}

saveGame :: String -> World -> IO ()
saveGame filename w = do putStrLn $ "Game saved to file: " ++ show filename
                         writeFile filename (show w)

module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
import Board
import AI
import Debug.Trace

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
handleInput :: Event -> World -> IO World
handleInput (EventMotion (x, y)) w = return w {mousePos = pos}
    where pos = screenSpaceToBoardSpace w (x, y)

-- Initiates check to see if placement is valid
-- Passes in point that's closest to a valid placement coord
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w = case maybepos of
    Just pos  -> return $ maybeBoardToWorld w $ makeMove (board w) (turn w) pos
    Nothing   -> return w
    where maybepos = mousePos w

handleInput (EventKey (Char 'u') Down _ _) w = return $ undo 2 w -- Undo twice to get back to player's move
handleInput (EventKey (Char 's') Down _ _) w = do saveGame "gomoku.save" w
                                                  return w
handleInput (EventKey (Char k) Down _ _) w = return $ trace ("Is there a win? " ++ show (checkWon w)) w
handleInput (EventKey (Char k) Up _ _) w = return w
handleInput e w = return w

saveGame :: String -> World -> IO ()
saveGame filename w = do putStrLn $ "Game saved to file: " ++ show filename
                         writeFile filename (show w)

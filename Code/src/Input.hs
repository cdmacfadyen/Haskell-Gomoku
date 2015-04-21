module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Board
import AdvancedAI
import Debug.Trace

-- | Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
handleInput :: Event -> -- ^Event input to handle
               World -> -- ^Current 'World' state
               IO World -- ^Returns new IO 'World'
handleInput (EventMotion (x, y)) w = return w {mouse=(x,y),board = newboard}
    where newboard = (board w) {mouse_board = pos}
    	  pos = screenSpaceToBoardSpace w (x, y)

-- | Initiates check to see if placement is valid
-- Passes in point that's closest to a valid placement coord
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w = case maybepos of
    Just pos  -> if (is_in_progress_game w) == True then return $ maybeBoardToWorld w $ makeMove (board w) (turn w) pos else return w
    Nothing   -> if (pointInBox (x,y) (384,180) (596,119))
                    then return $ undo 2 w
                    else
		       if (pointInBox (x,y) (381,80) (565,18))
                          then 
                             do saveGame "gomoku.save" w
                                return w
                          else 
                             if (pointInBox (x,y) (384,(-20)) (566,(-81)))
                                then return w{board = new_board, turn = Black, settings = new_settings}
                                else
                                   if (pointInBox (x,y) ((-539),137) ((-397),(99))) 
                                      then return $ handle_grid_size (x,y) w 
                                      else
                                         if (pointInBox (x,y) ((-513),(-10)) ((-425),(-43)))
                                            then return $ handle_target_size (x,y) w 
                                            else
                                               if (pointInBox (x,y) ((-569),(-158)) ((-370),(-193)))
                                                  then return $ handle_ai_difficulty (x,y) w
                                                  else
                                                     if (pointInBox (x,y) ((-514),(-310)) ((-424),(-344)))
                                                        then return $ handle_your_colour (x,y) w
                                                        else
                                                           if (pointInBox (x,y) ((-560),(331)) ((-383),(270)))
                                                              then return $ start_game w
                                                              else
                                                                 if (pointInBox (x,y) ((382),(-122)) ((567),(-178)))
                                                                    then return w {board = newboard}
                                                                    else return w 
    where maybepos = mouse_board (board w)
    	  new_board = (board w){pieces = [], won = Nothing, hint = Nothing}
    	  new_settings = (settings w){game_in_progress=True}
    	  newboard = (board w) {hint = Just (getbestmove (board w) 1 (turn w))}
handleInput e w = return w

-- | When clicked on screen though GUI, this function is called which takes a
-- default file name, the 'World' state at the time of saving, and returns IO.
saveGame :: String -> -- ^Takes name of file as 'String'
            World -> -- ^Current 'World' state
            IO () -- ^Returns IO
saveGame filename w = do putStrLn $ "Game saved to file: " ++ show filename
                         writeFile filename (show w)

-- | Function to recognise where on the screen you click to set the grid size. 
-- On screen options are shown through GUI
handle_grid_size :: (Float,Float) -> -- ^Takes position of screen click
                    World -> -- ^Current 'World' state
                    World -- ^Returns new 'World' state
handle_grid_size (x,y) w = if (pointInBox (x,y) ((-539),137) ((-501),(101)))
                              then w{settings = (change_grid_size (settings w) 2)}
                              else
                                 if (pointInBox (x,y) ((-487),136) ((-449),(101)))
                                    then w{settings = (change_grid_size (settings w) 5)}
                                    else
                                       if (pointInBox (x,y) ((-439),138) ((-397),(99)))
                                          then w{settings = (change_grid_size (settings w) 18)}
                                          else w
-- | Changes the original grid size in 'Settings' with a new grid size passed
-- in as 'Int'
change_grid_size :: Settings -> -- ^Takes current 'Settings'
                    Int -> -- ^'Int' that represents new grid size
                    Settings -- ^Returns new 'Settings' with new grid size
change_grid_size settings size = settings{grid_size = size}

-- | Function to recognise where on the screen you click to set the target size. 
-- On screen options are shown through GUI
handle_target_size :: (Float,Float) -> -- ^Takes position of screen click
                      World -> -- ^Current 'World' state
                      World -- ^Returns new 'World' state
handle_target_size (x,y) w = if (pointInBox (x,y) ((-513),(-10)) ((-475),(-42)))
                                then w{settings = (change_target_size (settings w) 3)}
                                else
                                   if (pointInBox (x,y) ((-466),(-10)) ((-425),(-43)))
                                      then w{settings = (change_target_size (settings w) 5)}
                                      else w

-- | Changes the original target size in 'Settings' with a new target size
-- passed in as 'Int'
change_target_size :: Settings -> -- ^Takes current 'Settings'
                      Int -> -- ^'Int' that represents new grid size
                      Settings -- ^Returns new 'Settings' with a new grid size
change_target_size settings size = settings{target_size = size}

-- | Function to recognise where on the screen you click to set the ai 
-- difficulty. On screen options are shown through GUI
handle_ai_difficulty :: (Float,Float) -> -- ^Takes position of screen click
                        World -> -- ^Current 'World' state
                        World -- ^Returns new 'World' state
handle_ai_difficulty (x,y) w = if (pointInBox (x,y) ((-569),(-158)) ((-509),(-194)))
                                  then w{settings = (change_ai_diff (settings w) 1)}
                                  else
                                     if (pointInBox (x,y) ((-498),(-159)) ((-442),(-192)))
                                        then w{settings = (change_ai_diff (settings w) 2)}
                                        else
                                           if (pointInBox (x,y) ((-429),(-161)) ((-370),(-193)))
                                              then w{settings = (change_ai_diff (settings w) 3)}
                                              else w

-- | Changes the original AI difficulty in 'Settings' with a new difficulty
-- level passed in as 'Int'
change_ai_diff :: Settings -> -- ^Takes current 'Settings'
                  Int -> -- ^'Int' that represents new ai difficulty
                  Settings -- ^Returns new 'Settings' with a new difficulty level
change_ai_diff settings diff = settings{ai_difficulty = diff}

-- | Function to recognise where on the screen you click to set the 'Colour'
-- you want to play. On screen options are shown through GUI 
handle_your_colour :: (Float,Float) -> -- ^Takes position of screen click
                      World -> -- ^Current 'World' state
                      World -- ^Returns new 'World' state
handle_your_colour (x,y) w = if (pointInBox (x,y) ((-514),(-310)) ((-475),(-345)))
                                then w{settings = (change_colour (settings w) (Just Black))}
                                else
                                   if (pointInBox (x,y) ((-465),(-309)) ((-424),(-344)))
                                      then w{settings = (change_colour (settings w) (Just White))}
                                      else w

-- | Changes the original 'Colour' with a new 'Colour' in 'Settings'
change_colour :: Settings -> -- ^Takes current 'Settings'
                 Maybe Colour -> -- ^'Colour' you want to play as
                 Settings -- ^Returns new 'Settings' with 'Colour' selection
change_colour settings col = settings{human_colour = col}

-- | Takes the current "empty" 'World' state and returns a new, initialised one
start_game :: World -> -- ^Takes current 'World' state
              World -- ^Returns new 'World' state
start_game w = World (start_board w) Black (0,0) (human_choice) (switch human_choice) new_settings 600
	where 
           human_choice = fromJust (human_colour (settings w))
           new_settings = (settings w){game_in_progress = True, configured = True}

-- | Returns 'Board' state from the current 'World' state
start_board :: World -> -- ^Takes current 'World' state
               Board -- ^Returns new 'Board' state
start_board w = (Board (grid_size (settings w)) (target_size (settings w)) Nothing Nothing [] Nothing)

-- | Function to extract the value from "Just a"
fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

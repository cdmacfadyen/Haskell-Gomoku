module Input(handleInput) where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Board
import AdvancedAI
import Debug.Trace


-- Debug aid
countTotals :: World -> String
countTotals w = show [countNConnected (board w) n (turn w) | n <- [2..6]]

-- Update the world state given an input event. Some sample input events
-- are given; when they happen, there is a trace printed on the console
handleInput :: Event -> World -> IO World
handleInput (EventMotion (x, y)) w = return w {mouse=(x,y),board = newboard}
    where newboard = (board w) {mouse_board = pos}
    	  pos = screenSpaceToBoardSpace w (x, y)

-- Initiates check to see if placement is valid
-- Passes in point that's closest to a valid placement coord
handleInput (EventKey (MouseButton LeftButton) Up m (x, y)) w = case maybepos of
    Just pos  -> if (is_in_progress_game w) == True then return $ maybeBoardToWorld w $ makeMove (board w) (turn w) pos else return w
    Nothing   -> if (pointInBox (x,y) (380,175) (521,125))
							then return $ undo 2 w
							else
								if (pointInBox (x,y) (380,74) (521,24))
									then 
										do saveGame "gomoku.save" w
										   return w
									else 
										if (pointInBox (x,y) (380,(-25)) (521,(-75)))
											then return w{board = new_board, turn = Black, settings = new_settings}
											else
												if (pointInBox (x,y) ((-520),133) ((-378),(106))) 
													then return $ handle_grid_size (x,y) w 
													else
														if (pointInBox (x,y) ((-495),(-13)) ((-402),(-43)))
															then return $ handle_target_size (x,y) w 
															else
																if (pointInBox (x,y) ((-495),(-163)) ((-402),(-192)))
																	then return $ handle_ai_difficulty (x,y) w
																	else
																		if (pointInBox (x,y) ((-520),(-316)) ((-375),(-345)))
																			then return $ handle_your_colour (x,y) w
																			else
																				if (pointInBox (x,y) ((-496),(317)) ((-404),(281)))
																					then return $ start_game w
																					else return w 
    where maybepos = mouse_board (board w)
    	  new_board = (board w){pieces = [], won = Nothing}
    	  new_settings = (settings w){game_in_progress=True}

handleInput (EventKey (Char 'c') Down _ _) w = trace (countTotals w) $ return w
handleInput (EventKey (Char 'e') Down _ _) w = trace (show $ evaluate (board w) (turn w)) $ return w
handleInput (EventKey (Char 'u') Down _ _) w = return $ undo 2 w -- Undo twice to get back to player's move
handleInput (EventKey (Char 's') Down _ _) w = do saveGame "gomoku.save" w
                                                  return w
handleInput (EventKey (Char 'h') Down _ _) w = return w {board = newboard}                                                
	where newboard = (board w) {hint = Just (getbestmove (board w) 1 (turn w))}
handleInput (EventKey (Char k) Down _ _) w = return $ trace ("Is there a win? " ++ show (won $ board w)) w
handleInput (EventKey (Char k) Up _ _) w = return w
handleInput e w = return w

saveGame :: String -> World -> IO ()
saveGame filename w = do putStrLn $ "Game saved to file: " ++ show filename
                         writeFile filename (show w)

handle_grid_size :: (Float,Float) -> World -> World
handle_grid_size (x,y) w = if (pointInBox (x,y) ((-520),133) ((-478),(105)))
							then w{settings = (change_grid_size (settings w) 3)}
							else
								if (pointInBox (x,y) ((-470),133) ((-428),(106)))
									then w{settings = (change_grid_size (settings w) 6)}
									else
										if (pointInBox (x,y) ((-420),133) ((-378),(106)))
											then w{settings = (change_grid_size (settings w) 19)}
											else w

change_grid_size :: Settings -> Int -> Settings
change_grid_size settings size = settings{grid_size = size}

handle_target_size :: (Float,Float) -> World -> World
handle_target_size (x,y) w = if (pointInBox (x,y) ((-495),(-13)) ((-454),(-40)))
							then w{settings = (change_target_size (settings w) 3)}
							else
								if (pointInBox (x,y) ((-445),(-12)) ((-402),(-43)))
									then w{settings = (change_target_size (settings w) 6)}
									else w

change_target_size :: Settings -> Int -> Settings
change_target_size settings size = settings{target_size = size}

handle_ai_difficulty :: (Float,Float) -> World -> World
handle_ai_difficulty (x,y) w = if (pointInBox (x,y) ((-495),(-163)) ((-453),(-190)))
							then w{settings = (change_ai_diff (settings w) 1)}
							else
								if (pointInBox (x,y) ((-443),(-161)) ((-402),(-192)))
									then w{settings = (change_ai_diff (settings w) 2)}
									else w

change_ai_diff :: Settings -> Int -> Settings
change_ai_diff settings diff = settings{ai_difficulty = diff}

handle_your_colour :: (Float,Float) -> World -> World
handle_your_colour (x,y) w = if (pointInBox (x,y) ((-520),(-316)) ((-476),(-346)))
							then w{settings = (change_colour (settings w) (Just Black))}
							else
								if (pointInBox (x,y) ((-421),(-314)) ((-375),(-345)))
									then w{settings = (change_colour (settings w) (Just White))}
									else w

change_colour :: Settings -> Maybe Colour -> Settings
change_colour settings col = settings{human_colour = col}

start_game :: World -> World
start_game w = World (start_board w) Black (0,0) (human_choice) (switch human_choice) new_settings 600
	where 
		human_choice = fromJust (human_colour (settings w))
		new_settings = (settings w){game_in_progress = True}

start_board :: World -> Board
start_board w = (Board (grid_size (settings w)) (target_size (settings w)) Nothing Nothing [] Nothing)

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

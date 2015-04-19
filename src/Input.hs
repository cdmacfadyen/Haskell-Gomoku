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
    Nothing   -> if trace (show (x,y)) (pointInBox (x,y) (384,180) (596,119))
							then return $ undo 2 w
							else
								if trace (show (x,y)) (pointInBox (x,y) (381,80) (565,18))
									then 
										do saveGame "gomoku.save" w
										   return w
									else 
										if trace (show (x,y)) (pointInBox (x,y) (384,(-20)) (566,(-81)))
											then return w{board = new_board, turn = Black, settings = new_settings}
											else
												if trace (show (x,y)) (pointInBox (x,y) ((-539),137) ((-397),(99))) 
													then return $ handle_grid_size (x,y) w 
													else
														if trace (show (x,y)) (pointInBox (x,y) ((-513),(-10)) ((-425),(-43)))
															then return $ handle_target_size (x,y) w 
															else
																if trace (show (x,y)) (pointInBox (x,y) ((-569),(-158)) ((-370),(-193)))
																	then return $ handle_ai_difficulty (x,y) w
																	else
																		if trace (show (x,y)) (pointInBox (x,y) ((-514),(-310)) ((-424),(-344)))
																			then return $ handle_your_colour (x,y) w
																			else
																				if trace (show (x,y)) (pointInBox (x,y) ((-560),(331)) ((-383),(270)))
																					then return $ start_game w
																					else
																						if trace (show (x,y)) (pointInBox (x,y) ((382),(-122)) ((567),(-178)))
																							then trace (show (x,y)) return w {board = newboard}
																							else trace (show (x,y)) return w 
    where maybepos = mouse_board (board w)
    	  new_board = (board w){pieces = [], won = Nothing, hint = Nothing}
    	  new_settings = (settings w){game_in_progress=True}
    	  newboard = (board w) {hint = Just (getbestmove (board w) 1 (turn w))}

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
handle_grid_size (x,y) w = if (pointInBox (x,y) ((-539),137) ((-501),(101)))
							then w{settings = (change_grid_size (settings w) 3)}
							else
								if (pointInBox (x,y) ((-487),136) ((-449),(101)))
									then w{settings = (change_grid_size (settings w) 6)}
									else
										if (pointInBox (x,y) ((-439),138) ((-397),(99)))
											then w{settings = (change_grid_size (settings w) 19)}
											else w

change_grid_size :: Settings -> Int -> Settings
change_grid_size settings size = settings{grid_size = size}

handle_target_size :: (Float,Float) -> World -> World
handle_target_size (x,y) w = if (pointInBox (x,y) ((-513),(-10)) ((-475),(-42)))
							then w{settings = (change_target_size (settings w) 3)}
							else
								if (pointInBox (x,y) ((-466),(-10)) ((-425),(-43)))
									then w{settings = (change_target_size (settings w) 6)}
									else w

change_target_size :: Settings -> Int -> Settings
change_target_size settings size = settings{target_size = size}

handle_ai_difficulty :: (Float,Float) -> World -> World
handle_ai_difficulty (x,y) w = if (pointInBox (x,y) ((-569),(-158)) ((-509),(-194)))
							then w{settings = (change_ai_diff (settings w) 1)}
							else
								if (pointInBox (x,y) ((-498),(-159)) ((-442),(-192)))
									then w{settings = (change_ai_diff (settings w) 2)}
									else
										if (pointInBox (x,y) ((-429),(-193)) ((-370),(-193)))
											then w{settings = (change_ai_diff (settings w) 3)}
											else w

change_ai_diff :: Settings -> Int -> Settings
change_ai_diff settings diff = settings{ai_difficulty = diff}

handle_your_colour :: (Float,Float) -> World -> World
handle_your_colour (x,y) w = if (pointInBox (x,y) ((-514),(-310)) ((-475),(-345)))
							then w{settings = (change_colour (settings w) (Just Black))}
							else
								if (pointInBox (x,y) ((-465),(-309)) ((-424),(-344)))
									then w{settings = (change_colour (settings w) (Just White))}
									else w

change_colour :: Settings -> Maybe Colour -> Settings
change_colour settings col = settings{human_colour = col}

start_game :: World -> World
start_game w = World (start_board w) Black (0,0) (human_choice) (switch human_choice) new_settings 600
	where 
		human_choice = fromJust (human_colour (settings w))
		new_settings = (settings w){game_in_progress = True, configured = True}

start_board :: World -> Board
start_board w = (Board (grid_size (settings w)) (target_size (settings w)) Nothing Nothing [] Nothing)

fromJust :: Maybe a -> a
fromJust Nothing = error "Maybe.fromJust: Nothing"
fromJust (Just x) = x

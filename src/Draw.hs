module Draw(drawWorld) where

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Board
import Debug.Trace

-- | Overall Draw Function.
drawWorld :: World -> Picture -> Picture -> Picture -> Picture 
			-> Picture -> Picture -> Picture -> Picture -> Picture 
			-> Picture -> Picture -> Picture -> Picture -> Picture 
			-> Picture -> Picture -> Picture -> Picture -> Picture 
			-> Picture -> Picture -> Picture -> Picture
drawWorld world background black_p white_p undo save undo_h save_h restart restart_h thinking ai_difficulty black done grid_size nineteen six target_size three white colour_button black_won white_won = 
																			   Pictures[background, drawBoard world, 
																			   drawPieces world black_p white_p, 
																			   highlight world, draw_hint world,
																			   draw_undo (mouse world) undo undo_h,
																			   draw_save (mouse world) save save_h,
																			   draw_restart (mouse world) restart restart_h,
																			   draw_ai_think world thinking, draw_grid_txt world grid_size,
																			   draw_target_txt world target_size, draw_ai_txt world ai_difficulty,
																			   draw_your_colour world colour_button, check_done world (mouse world) done,
																			   is_draw_settings world three six white black black_won white_won,
																			   draw_winner world black_won white_won, draw_hints_btn (mouse world) undo undo_h]
-- | Draws the board.
drawBoard :: World -> Picture
drawBoard world = Pictures[Color (greyN 0.6) . genGrid world . size $ board world]

-- Generates the grid of size count for the given world, by calling horizontal and vertical, and then translating the resultant grid by negative half of the screen width on each axis.
genGrid :: World -> Int -> Picture
genGrid world count = Translate (-screensize / 2) (-screensize / 2) $ Pictures $ horizontal gridsize count ++ vertical gridsize count
    where gridsize = squareSize world
          screensize = fromIntegral $ width world

-- Generates count horizontal lines gridsize apart, starting from the origin.
horizontal :: Float -> Int -> [Picture]
horizontal gridsize count = [Line [(0 :: Float, gridsize * n), (gridsize * fromIntegral count, gridsize * n)] | n <- map fromIntegral [0..count] :: [Float]]

-- Generates count vertical lines gridsize apart, starting from the origin.
vertical :: Float -> Int -> [Picture]
vertical gridsize count = [Line [(gridsize * n, 0 :: Float), (gridsize * n, gridsize * fromIntegral count)] | n <- map fromIntegral [0..count] :: [Float]]

drawPieces :: World -> Picture -> Picture -> Picture
drawPieces world black_piece white_piece = Pictures [drawPiece world piece black_piece white_piece | piece <- pieces $ board world]

drawPiece :: World -> (Position, Colour) -> Picture -> Picture -> Picture
drawPiece world (position, col) black_p white_p = uncurry Translate (boardSpaceToScreenSpace world position) $ Scale scaled scaled (colour_piece col black_p white_p)
								where scaled = (squareSize world) / 420

colour_piece :: Colour -> Picture -> Picture -> Picture
colour_piece col black_p white_p = if col == Black then black_p else white_p

highlight :: World -> Picture
highlight w = case mousep of 
		           Just p  -> if (game_in_progress (settings w)) == True 
		           				then if contains p $ pieces $ board w 
		           						then Blank 
		           						else 
		           							if (is_in_progress_game w) == True then drawHighlight w p else Blank
		           				else Blank
		           Nothing -> Blank
    where mousep = mouse_board (board w)

drawHighlight :: World -> Position -> Picture
drawHighlight w p = Color (greyN 0.2) $ uncurry Translate (boardSpaceToScreenSpace w p) $ thickCircle (squareSize w / 2) 9

draw_hint :: World -> Picture
draw_hint w  = case (hint (board w)) of
					Just pos -> if (is_in_progress_game w)
									then Color red $ uncurry Translate (boardSpaceToScreenSpace w pos) $ thickCircle (squareSize w / 2) 9
									else trace (show "nope") Blank
					Nothing  -> Blank

draw_undo :: (Float,Float) -> Picture -> Picture -> Picture
draw_undo (x,y) undo undo_h = if (pointInBox (x,y) (380,175) (521,125))
								then Translate 450 150 $ undo_h
								else Translate 450 150 $ undo

draw_save :: (Float,Float) -> Picture -> Picture -> Picture
draw_save (x,y) save save_h = if (pointInBox (x,y) (380,74) (521,24))
								then Translate 450 50 $ save_h
								else Translate 450 50 $ save

draw_restart :: (Float,Float) -> Picture -> Picture -> Picture
draw_restart (x,y) restart restart_h = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate 450 (-50) $ restart_h
								else Translate 450 (-50) $ restart

draw_hints_btn :: (Float,Float) -> Picture -> Picture -> Picture
draw_hints_btn (x,y) hints hints_h = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate 450 (-150) $ hints
								else Translate 450 (-150) $ hints_h

is_draw_settings :: World -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
is_draw_settings world three six white black black_won white_won = if (game_in_progress (settings world)) /= True
																			then Pictures [draw_size_three (mouse world) three, draw_size_six (mouse world) six, 
																			   draw_size_nineteen (mouse world) six,draw_target_three (mouse world) three, draw_target_six (mouse world) six,
																			   draw_ai_one (mouse world) three, draw_ai_two (mouse world) six,
																			   draw_white_button (mouse world) white, draw_black_button(mouse world) black]
																			else Blank

draw_ai_think :: World -> Picture -> Picture
draw_ai_think world pict = if ((turn world) == (computer world)) && (won (board world)) /= Just (switch(turn world))
						then pict
						else Blank

draw_grid_txt :: World ->  Picture -> Picture
draw_grid_txt w pict = if ((game_in_progress (settings w)) /= True) then Translate (-450) 200 $ pict else Blank

draw_target_txt :: World -> Picture -> Picture
draw_target_txt w pict = if ((game_in_progress (settings w)) /= True) then Translate (-450) 50 $ pict else Blank

draw_ai_txt :: World -> Picture -> Picture
draw_ai_txt w pict = if ((game_in_progress (settings w)) /= True) then Translate (-450) (-100) $ pict else Blank

draw_your_colour :: World -> Picture -> Picture
draw_your_colour w pict = if ((game_in_progress (settings w)) /= True) then Translate (-450) (-250) $ pict else Blank

draw_size_three :: (Float,Float) -> Picture -> Picture
draw_size_three (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-500) 120 $ pict
								else Translate (-500) 120 $ pict

draw_size_six :: (Float,Float) -> Picture -> Picture
draw_size_six (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-450) 120 $ pict
								else Translate (-450) 120 $ pict

draw_size_nineteen :: (Float,Float) -> Picture -> Picture
draw_size_nineteen (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-400) 120 $ pict
								else Translate (-400) 120 $ pict

draw_target_three :: (Float,Float) -> Picture -> Picture
draw_target_three  (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-475) (-25) $ pict
								else Translate (-475) (-25)$ pict

draw_target_six :: (Float,Float) -> Picture -> Picture
draw_target_six (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-425) (-25) $ pict
								else Translate (-425) (-25) $ pict

draw_ai_one :: (Float,Float) -> Picture -> Picture
draw_ai_one (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-475) (-175) $ pict
								else Translate (-475) (-175) $ pict

draw_ai_two :: (Float,Float) -> Picture -> Picture
draw_ai_two (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-425) (-175) $ pict
								else Translate (-425) (-175) $ pict

draw_black_button :: (Float,Float) -> Picture -> Picture
draw_black_button (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-475) (-325) $ pict
								else Translate (-475) (-325) $ pict

draw_white_button :: (Float,Float) -> Picture -> Picture
draw_white_button (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
								then Translate (-375) (-325) $ pict
								else Translate (-375) (-325) $ pict

check_done :: World -> (Float,Float) -> Picture -> Picture
check_done w (x,y) pict = if (settings_complete (settings w))
							then draw_done set (x,y) pict
							else Blank
				where set = (settings w){configured=True}

draw_done :: Settings -> (Float,Float) -> Picture -> Picture
draw_done set (x,y) pict = if (pointInBox (x,y) (380,(-25)) (521,(-75)))
							&& ((game_in_progress set) /= True)
							then Translate (-450) 300 $ pict
							else 
								if ((game_in_progress set) /= True)
									then Translate (-450) 300 $ pict
									else Blank

draw_winner :: World -> Picture -> Picture -> Picture
draw_winner w black_won white_won = case (won (board w)) of
			Just Black -> black_won
			Just White -> white_won
			Nothing -> Blank


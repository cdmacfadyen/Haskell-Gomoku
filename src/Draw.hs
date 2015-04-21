module Draw(drawWorld) where

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Board
import Debug.Trace

-- | Overall Draw Function.
drawWorld :: World -> Picture -> Picture -> Picture -> Picture ->
			 Picture -> Picture -> Picture -> Picture -> Picture ->
			 Picture -> Picture -> Picture -> Picture -> Picture ->
			 Picture -> Picture -> Picture -> Picture -> Picture ->
			 Picture -> Picture -> Picture -> Picture -> Picture ->
			 Picture -> Picture -> Picture -> Picture -> Picture ->
			 Picture -> Picture -> Picture -> Picture -> Picture ->
			 Picture -> Picture -> Picture -> Picture -> Picture
drawWorld world background black_p white_p undo save undo_h save_h 
   restart restart_h thinking ai_difficulty black done grid_size nineteen six 
      target_size three white colour_button black_won white_won hint_button hint_h 
         done_h black_h white_h three_h six_h nineteen2 nineteen_h easy easy_h hard 
            hard_h med med_h tied = Pictures[background, drawBoard world, 
                                    drawPieces world black_p white_p, 
                                    highlight world, draw_hint world,
                                    draw_undo (mouse world) undo undo_h,
                                    draw_save (mouse world) save save_h,
                                    draw_restart (mouse world) restart restart_h,
                                    draw_ai_think world thinking, draw_grid_txt world grid_size,
                                    draw_target_txt world target_size, draw_ai_txt world ai_difficulty,
                                    draw_your_colour world colour_button, 
                                    check_done world (mouse world) done done_h,
                                    is_draw_settings world three six nineteen2 
                                       white black black_won white_won easy easy_h 
                                    hard hard_h med med_h three_h six_h nineteen_h white_h black_h, 
                                    draw_winner world black_won white_won tied, 
                                    draw_hints_btn (mouse world) hint_button hint_h]
-- | Draws the board.
drawBoard :: World -> -- ^Takes a current 'World' state
             Picture -- ^Returns a 'Picture'
drawBoard world = Pictures[Color (greyN 0.6) . genGrid world . size $ board world]

-- | Generates the grid of size count for the given world, by calling 
-- horizontal and vertical, and then translating the resultant grid by
-- negative half of the screen width on each axis.
genGrid :: World -> -- ^Takes current 'World' state
           Int -> -- ^The size of the desired grid
           Picture -- ^'Picture' of the generated 'World'
genGrid world count = Translate (-screensize / 2) (-screensize / 2) $ 
                         Pictures $ horizontal gridsize count ++ vertical gridsize count
    where gridsize = squareSize world
          screensize = fromIntegral $ width world

-- | Generates count horizontal lines gridsize apart, starting from the origin.
horizontal :: Float -> -- ^The origin
              Int -> -- ^Number of horizontal lines
              [Picture] -- ^Returns list of 'Picture' representing horizontal lines
horizontal gridsize count = [Line [(0 :: Float, gridsize * n), 
                              (gridsize * fromIntegral count, gridsize * n)] 
                                | n <- map fromIntegral [0..count] :: [Float]]

-- Generates count vertical lines gridsize apart, starting from the origin.
vertical :: Float -> -- ^The origin
            Int -> -- ^Number of vertical lines
            [Picture] -- ^Returns list of 'Picture' representing vertical lines
vertical gridsize count = [Line [(gridsize * n, 0 :: Float), 
                            (gridsize * n, gridsize * fromIntegral count)] 
                              | n <- map fromIntegral [0..count] :: [Float]]

-- | Function that iterates over pieces in 'Board' and returns 'Picture' of
-- the drawn pieces
drawPieces :: World -> Picture -> Picture -> Picture
drawPieces world black_piece white_piece = Pictures [drawPiece world 
                                                      piece black_piece white_piece 
                                                       | piece <- pieces $ board world]

-- | Funciton that takes a 'World' state and 
drawPiece :: World -> -- ^Takes current 'World' state
             (Position, Colour) -> -- ^Tuple containing co-ordinate and
                                   -- respective 'Colour'
             Picture -> -- ^
             Picture -> 
             Picture
drawPiece world (position, col) black_p white_p 
  = uncurry Translate (boardSpaceToScreenSpace world position) $ 
       Scale scaled scaled (colour_piece col black_p white_p)
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
drawHighlight w p = Color (greyN 0.25) $ uncurry Translate (boardSpaceToScreenSpace w p) $ thickCircle (squareSize w / 2) 10

draw_hint :: World -> Picture
draw_hint w  = case (hint (board w)) of
					Just pos -> if (is_in_progress_game w)
									then Color red $ uncurry Translate (boardSpaceToScreenSpace w pos) $ thickCircle (squareSize w / 2) 9
									else Blank
					Nothing  -> Blank

draw_undo :: (Float,Float) -> Picture -> Picture -> Picture
draw_undo (x,y) undo undo_h = if (pointInBox (x,y) (384,180) (596,119))
								then Translate 475 150 $ undo_h
								else Translate 475 150 $ undo

draw_save :: (Float,Float) -> Picture -> Picture -> Picture
draw_save (x,y) save save_h = if (pointInBox (x,y) (381,80) (565,18))
								then Translate 475 50 $ save_h
								else Translate 475 50 $ save

draw_restart :: (Float,Float) -> Picture -> Picture -> Picture
draw_restart (x,y) restart restart_h = if (pointInBox (x,y) (384,(-20)) (566,(-81)))
								then Translate 475 (-50) $ restart_h
								else Translate 475 (-50) $ restart

draw_hints_btn :: (Float,Float) -> Picture -> Picture -> Picture
draw_hints_btn (x,y) hints hints_h = if (pointInBox (x,y) ((382),(-122)) ((567),(-178)))
								then Translate 475 (-150) $ hints_h
								else Translate 475 (-150) $ hints

is_draw_settings :: World -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
is_draw_settings world three six nineteen white black black_won white_won easy easy_h hard hard_h med med_h three_h six_h nineteen_h white_h black_h = if (game_in_progress (settings world)) /= True
																			then Pictures [draw_size_three (mouse world) three three_h , draw_size_six (mouse world) six six_h, 
																			   draw_size_nineteen (mouse world) nineteen nineteen_h, draw_target_three (mouse world) three three_h, draw_target_six (mouse world) six six_h,
																			   draw_ai_one (mouse world) easy easy_h, draw_ai_two (mouse world) med med_h, draw_ai_three (mouse world) hard hard_h,
																			   draw_white_button (mouse world) white white_h, draw_black_button(mouse world) black black_h]
																			else Blank

draw_ai_think :: World -> Picture -> Picture
draw_ai_think world pict = if ((turn world) == (computer world)) && (won (board world)) /= Just (switch(turn world))
						then pict
						else Blank

draw_grid_txt :: World ->  Picture -> Picture
draw_grid_txt w pict = if ((game_in_progress (settings w)) /= True) then Translate (-470) 200 $ pict else Blank

draw_target_txt :: World -> Picture -> Picture
draw_target_txt w pict = if ((game_in_progress (settings w)) /= True) then Translate (-470) 50 $ pict else Blank

draw_ai_txt :: World -> Picture -> Picture
draw_ai_txt w pict = if ((game_in_progress (settings w)) /= True) then Translate (-470) (-100) $ pict else Blank

draw_your_colour :: World -> Picture -> Picture
draw_your_colour w pict = if ((game_in_progress (settings w)) /= True) then Translate (-470) (-250) $ pict else Blank

draw_size_three :: (Float,Float) -> Picture -> Picture -> Picture
draw_size_three (x,y) pict pict_h = if (pointInBox (x,y) ((-539),137) ((-501),(101)))
								then Translate (-520) 120 $ pict_h
								else Translate (-520) 120 $ pict

draw_size_six :: (Float,Float) -> Picture -> Picture -> Picture
draw_size_six (x,y) pict pict_h = if (pointInBox (x,y) ((-487),136) ((-449),(101)))
								then Translate (-470) 120 $ pict_h
								else Translate (-470) 120 $ pict

draw_size_nineteen :: (Float,Float) -> Picture -> Picture -> Picture
draw_size_nineteen (x,y) pict pict_h = if (pointInBox (x,y) ((-439),138) ((-397),(99)))
								then Translate (-420) 120 $ pict_h
								else Translate (-420) 120 $ pict

draw_target_three :: (Float,Float) -> Picture -> Picture -> Picture
draw_target_three  (x,y) pict pict_h = if (pointInBox (x,y) ((-513),(-10)) ((-475),(-42)))
								then Translate (-495) (-25) $ pict_h
								else Translate (-495) (-25)$ pict

draw_target_six :: (Float,Float) -> Picture -> Picture -> Picture
draw_target_six (x,y) pict pict_h = if (pointInBox (x,y) ((-466),(-10)) ((-425),(-43)))
								then Translate (-445) (-25) $ pict_h
								else Translate (-445) (-25) $ pict

draw_ai_one :: (Float,Float) -> Picture -> Picture -> Picture
draw_ai_one (x,y) pict pict_h = if (pointInBox (x,y) ((-569),(-158)) ((-509),(-194)))
								then Translate (-540) (-175) $ pict_h
								else Translate (-540) (-175) $ pict

draw_ai_two :: (Float,Float) -> Picture -> Picture -> Picture
draw_ai_two (x,y) pict pict_h = if (pointInBox (x,y) ((-498),(-159)) ((-442),(-192)))
								then Translate (-470) (-175) $ pict_h
								else Translate (-470) (-175) $ pict

draw_ai_three :: (Float,Float) -> Picture -> Picture -> Picture
draw_ai_three (x,y) pict pict_h = if (pointInBox (x,y) ((-429),(-161)) ((-370),(-193)))
								then Translate (-400) (-175) $ pict_h
								else Translate (-400) (-175) $ pict

draw_black_button :: (Float,Float) -> Picture -> Picture -> Picture
draw_black_button (x,y) pict pict_h = if (pointInBox (x,y) ((-514),(-310)) ((-475),(-345)))
								then Translate (-495) (-325) $ pict_h
								else Translate (-495) (-325) $ pict

draw_white_button :: (Float,Float) -> Picture -> Picture -> Picture
draw_white_button (x,y) pict pict_h = if (pointInBox (x,y) ((-465),(-309)) ((-424),(-344)))
								then Translate (-445) (-325) $ pict_h
								else Translate (-445) (-325) $ pict

check_done :: World -> (Float,Float) -> Picture -> Picture -> Picture
check_done w (x,y) pict pict_h = if (settings_complete (settings w))
							then draw_done set (x,y) pict pict_h
							else Blank
				where set = (settings w){configured=True}

draw_done :: Settings -> (Float,Float) -> Picture -> Picture -> Picture
draw_done set (x,y) pict pict_h = if (pointInBox (x,y) ((-560),(331)) ((-383),(270)))
							&& ((game_in_progress set) /= True)
							then Translate (-470) 300 $ pict_h
							else 
								if ((game_in_progress set) /= True)
									then Translate (-470) 300 $ pict
									else Blank

draw_winner :: World -> Picture -> Picture -> Picture -> Picture
draw_winner w black_won white_won tied = case (won (board w)) of
			Just Black -> black_won
			Just White -> white_won
			Nothing -> if ((size (board w) + 1)^2) == (length (pieces (board w)))
						then tied
						else Blank


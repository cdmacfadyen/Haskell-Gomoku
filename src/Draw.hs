module Draw(drawWorld) where

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Board

-- | Overall Draw Function.
drawWorld :: World -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
drawWorld world background black_p white_p undo save undo_h save_h restart restart_h thinking ai_difficulty black done grid_size nineteen six target_size three white colour_button = Pictures[background, drawBoard world, 
																			   drawPieces world black_p white_p, 
																			   highlight world, draw_hint world,
																			   draw_undo (mouse world) undo undo_h,
																			   draw_save (mouse world) save save_h,
																			   draw_restart (mouse world) restart restart_h,
																			   draw_ai_think world thinking, draw_grid_txt grid_size,
																			   draw_target_txt target_size, draw_ai_txt ai_difficulty,
																			   draw_your_colour colour_button, draw_size_three (mouse world) three,
																			   draw_size_six (mouse world) six, draw_size_nineteen (mouse world) six,
																			   draw_target_three (mouse world) three, draw_target_six (mouse world) six,
																			   draw_ai_one (mouse world) three, draw_ai_two (mouse world) six,
																			   draw_white_button (mouse world) white, draw_black_button(mouse world) black,
																			   draw_done world (mouse world) done]
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
		           Just p  -> if contains p $ pieces $ board w then Blank else drawHighlight w p
		           Nothing -> Blank
    where mousep = mouse_board (board w)

drawHighlight :: World -> Position -> Picture
drawHighlight w p = Color (greyN 0.2) $ uncurry Translate (boardSpaceToScreenSpace w p) $ thickCircle (squareSize w / 2) 9

draw_hint :: World -> Picture
draw_hint w  = case (hint (board w)) of
					Just pos -> Color red $ uncurry Translate (boardSpaceToScreenSpace w pos) $ thickCircle (squareSize w / 2) 9
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

draw_ai_think :: World -> Picture -> Picture
draw_ai_think world pict = if ((turn world) == (computer world)) && (won (board world)) /= Just (switch(turn world))
						then pict
						else Blank

draw_grid_txt :: Picture -> Picture
draw_grid_txt pict = Translate (-450) 200 $ pict

draw_target_txt :: Picture -> Picture
draw_target_txt pict = Translate (-450) 50 $ pict

draw_ai_txt :: Picture -> Picture
draw_ai_txt pict = Translate (-450) (-100) $ pict

draw_your_colour :: Picture -> Picture
draw_your_colour pict = Translate (-450) (-250) $ pict

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

draw_done :: World -> (Float,Float) -> Picture -> Picture
draw_done w (x,y) pict = if (configured (settings w)) == True 
							then if (pointInBox (x,y) (380,(-25)) (521,(-75)))
									then Translate (-450) 300 $ pict
									else Translate (-450) 300 $ pict
							else Blank 


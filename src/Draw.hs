module Draw(drawWorld) where

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Board

-- | Overall Draw Function.
drawWorld :: World -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture
drawWorld world background black_p white_p undo save undo_h save_h = Pictures[background, drawBoard world, 
																			   drawPieces world black_p white_p, 
																			   highlight world, draw_hint world,
																			   draw_undo (mouse world) undo undo_h,
																			   draw_save (mouse world) save save_h]

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
    where mousep = mousePos (board w)

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

--draw_undo_btn :: World -> Picture -> Picture -> Picture
--draw_undo_btn w undo_btn undo_btn-h = case mousep of
--										Just p -> pointInBox (mousePos (382,175) (522,125)) then Translate 450 150 $ undo_btn-h else Translate 450 150 $ undo_btn
--										Nothing -> Blank
--	where mousep = mousePos (board w)

--draw_save_btn :: World -> Picture -> Picture -> Picture
--draw_save_btn w save_btn save_btn-h= Translate  450 50 $ save_btn 




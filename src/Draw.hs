module Draw(drawWorld) where

import Graphics.Gloss

import Board

-- | Overall Draw Function.
drawWorld :: World -> Picture
drawWorld world = Pictures[drawBoard world, drawPieces world]

-- | Draws the board.
drawBoard :: World -> Picture
drawBoard world = Pictures[Color red $ genGrid (fromIntegral(size $ board world))(fromIntegral(width world))]

-- | Number of cells in grid (cells) and the width of cells (width).
-- List comprehension to draw 'n' horizontal and vertical lines.
genGrid :: Float -> Float -> Picture
genGrid cells width = Pictures[Pictures [horizontal x (width/cells) (width/2) | x <- [-cells/2..cells/2]], 
			 Pictures [vertical x (width/cells) (width/2) | x <- [-cells/2..cells/2]]]

horizontal :: Float -> Float -> Float -> Picture
horizontal x sqrWidth scrWidth = Line[(-scrWidth, x*sqrWidth), (scrWidth, x*sqrWidth)] 

vertical :: Float -> Float -> Float -> Picture
vertical x sqrWidth scrWidth = Line[(x*sqrWidth, -scrWidth), (x*sqrWidth, scrWidth)] 

drawPieces :: World -> Picture
drawPieces w = Pictures [drawPiece piece (squareSize w)| piece <- pieces $ board w]

drawPiece :: (Position, Colour) -> Float -> Picture
drawPiece ((x, y), colour) size = Color (colourPiece colour) $ Translate (size * fromIntegral(x)) (size * fromIntegral(y)) (ThickCircle (size / 8) (size / 4))

colourPiece :: Colour -> Color
colourPiece Black = black
colourPiece White = white

squareSize :: World -> Float
squareSize w = fromIntegral(width w) / fromIntegral(size $ board w)








	

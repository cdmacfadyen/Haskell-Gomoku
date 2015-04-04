module Draw(drawWorld) where

import Graphics.Gloss

import Board

-- | Overall Draw Function.
drawWorld :: World -> Picture
drawWorld world = Pictures[drawBoard world, drawPieces world, highlight world (mousePos world)]

-- | Draws the board.
drawBoard :: World -> Picture
drawBoard world = Pictures[Color red . genGrid world . size $ board world]

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

drawPieces :: World -> Picture
drawPieces w = Pictures [drawPiece w piece | piece <- pieces $ board w]

drawPiece :: World -> (Position, Colour) -> Picture
drawPiece world (position, colour) = Color (colourPiece colour) $ uncurry Translate (boardSpaceToScreenSpace world position) $ ThickCircle (squareSize world / 8) (squareSize world / 4)

colourPiece :: Colour -> Color
colourPiece Black = black
colourPiece White = white

highlight :: World-> Maybe Position -> Picture
highlight w Nothing = Blank
highlight w (Just p) = if contains p $ pieces $ board w then Blank else drawHighlight w p

drawHighlight :: World -> Position -> Picture
drawHighlight world position  = Color (makeColor 0.2 0.3 0.4 0.5) $ uncurry Translate (boardSpaceToScreenSpace world position) $ thickCircle (150 * 0.275) 7

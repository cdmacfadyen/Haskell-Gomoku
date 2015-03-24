module Draw(drawWorld) where

import Graphics.Gloss

import Board

-- | Overall Draw Function.
drawWorld :: World -> Picture
drawWorld world = Pictures[drawBoard world, drawPieces world]

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
drawPieces w = Pictures [drawPiece piece (squareSize w)| piece <- pieces $ board w]

drawPiece :: (Position, Colour) -> Float -> Picture
drawPiece ((x, y), colour) size = Color (colourPiece colour) $ Translate (size * fromIntegral x) (size * fromIntegral y) $ ThickCircle (size / 8) (size / 4)

colourPiece :: Colour -> Color
colourPiece Black = black
colourPiece White = white

squareSize :: World -> Float
squareSize w = fromIntegral (width w) / (fromIntegral . size $ board w)

screenSpaceToBoardSpace :: World -> (Float, Float) -> Position
screenSpaceToBoardSpace world (screenx, screeny) = (round (screenx / gridsize), round (screeny / gridsize))
    where gridsize = squareSize world

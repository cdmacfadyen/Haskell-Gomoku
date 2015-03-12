module Draw(drawWorld) where

import Graphics.Gloss
import Board

-- Given a world state, return a Picture which will render the world state.
-- Currently just draws a single blue circle as a placeholder.
--
-- This will need to extract the Board from the world state and draw it
-- as a grid plus pieces.
drawWorld :: World -> Picture
drawWorld w = Pictures [Color red $ drawGrid (fromIntegral(size $ board w)) (fromIntegral(width w))]

-- Number of cells in grid (g) and the width of cells (w)
-- List comprehension to draw 'n' horizontal and vertical lines
drawGrid :: Float -> Float -> Picture
drawGrid g w = Pictures [Pictures [horizontalLine x (w/g) (w/2) | x <- [-g/2..g/2]], 
			 Pictures [verticalLine x (w/g) (w/2) | x <- [-g/2..g/2]]]

horizontalLine :: Float -> Float -> Float -> Picture
horizontalLine x sqrWidth scrWidth = Line [(-scrWidth, x*sqrWidth), (scrWidth, x*sqrWidth)] 

verticalLine :: Float -> Float -> Float -> Picture
verticalLine x sqrWidth scrWidth = Line [(x*sqrWidth, -scrWidth), (x*sqrWidth, scrWidth)] 

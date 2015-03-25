module Board where

-- | Player piece colour.
data Colour = Black | White
  deriving Show

-- | Switch current player.
switch :: Colour -> Colour
switch Black = White
switch White = Black

-- | Indicate a piece position of board.
type Position = (Int, Int)

-- | Indicate a piece position of board.
type Piece = (Position,Colour)

data Board = Board { size :: Int, -- ^ Board Size.
                     target :: Int, -- ^ Target 'in-a-row'
                     pieces :: [Piece] -- ^ Position List.
                   }
  			 deriving Show

data World = World { board :: Board, -- ^ Board Representation
                     turn :: Colour, -- ^ Current Player
		     		 width :: Int } -- ^ Width 


-- | Default board: 6x6, target is 3 in a row, no initial pieces
--initBoard = Board 6 3 [((1, 1), Black), ((-2, 3), White)] -- For testing purposes!
initBoard = Board 6 3 []

-- | Default world: initial board, black is current player.
initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Colour -> Position -> Maybe Board
makeMove b col p = if contains p $ pieces b then Nothing else Just b {pieces = ((p,col) : (pieces b))}

-- Checks if a piece on the board is in the position you're trying to place a piece
contains :: Position -> [Piece] -> Bool
contains coord [] = False
contains coord ((position, col):xs) 
	| coord == position = True
	| otherwise         = contains coord xs


-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Colour
checkWon = undefined

{- Hint: One way to implement 'checkWon' would be to write functions 
which specifically check for lines in all 8 possible directions
(NW, N, NE, E, W, SE, SW)

In these functions:
To check for a line of n in a row in a direction D:
For every position ((x, y), col) in the 'pieces' list:
- if n == 1, the colour 'col' has won
- if n > 1, move one step in direction D, and check for a line of
  n-1 in a row.
-}

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Colour -> Int
evaluate = undefined

squareSize :: World -> Float
squareSize w = fromIntegral (width w) / (fromIntegral . size $ board w)

-- Given a world and a screen-space co-ordinate, return the closest board position to that co-ordinate if it is in bounds, or Nothing if it is not.
screenSpaceToBoardSpace :: World -> (Float, Float) -> Maybe Position
screenSpaceToBoardSpace world (screenx, screeny) 
    | abs boardx <= halfgridwidth && abs boardy <= halfgridwidth = Just (boardx, boardy)
    | otherwise = Nothing
    where gridsize      = squareSize world
          boardx        = round $ screenx / gridsize
          boardy        = round $ screeny / gridsize
          halfgridwidth = (size (board world)) `quot` 2

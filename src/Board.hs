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
-- initBoard '=' Board 6 3 [] 
initBoard = Board 6 3 [((1, 1), Black), ((-2, 3), White)] -- For testing purposes!

-- | Default world: initial board, black is current player.
initWorld = World initBoard Black

-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Colour -> Position -> Maybe Board
makeMove = undefined

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




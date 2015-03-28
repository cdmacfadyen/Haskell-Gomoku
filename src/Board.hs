module Board where

-- | Player piece colour.
data Colour = Black | White
  deriving (Show, Eq)

-- | Switch current player.
switch :: Colour -> Colour
switch Black = White
switch White = Black

-- | Indicate a piece position of board.
type Position = (Int, Int)

-- | Indicate a piece position of board.
type Piece = (Position, Colour)

data Board = Board { size :: Int, -- ^ Board Size.
                     target :: Int, -- ^ Target 'in-a-row'
                     pieces :: [Piece], -- ^ Position List.
                   	 won :: Bool } -- ^ Win Status.
         deriving Show

data World = World { board :: Board, -- ^ Board Representation
                     mousePos :: Maybe Position,
                     turn :: Colour, -- ^ Current Player
             		 width :: Int } -- ^ Width

-- | Default board: 6x6, target is 3 in a row, no initial pieces
initBoard = Board 6 3 [] False

-- | Default world: initial board, black is current player.
initWorld = World initBoard Nothing Black

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
checkWon :: World -> Maybe Colour
checkWon world = if (win (genWinList (size (board world)) (target (board world))) (board world) (switch (turn world))) 
					then Just (switch (turn world)) 
					else Nothing

-- Generates a list of 'winning patterns'.
genWinList :: Int -> Int -> [[Position]]
genWinList size target = (genVertical size) ++ (genHorizontal size) ++ (genDiagonal size target)

-- Generates a list of vertical win patterns.
genVertical :: Int -> [[Position]]
genVertical size = [[(x,y),(x,y+1),(x,y+2)] | x <- [-size..(size)], y <- [-size..size]]

-- Generates a list of horizontal win patterns.
genHorizontal :: Int -> [[Position]]
genHorizontal size = [[(x,y),(x+1,y),(x+2,y)] | x <- [-size..size], y <- [-size..size]]

-- Generates a list of diagonal win patterns.
genDiagonal :: Int -> Int -> [[Position]]
genDiagonal size target = [[(x,y),(x+1,y+1),(x+2,y+2)] 
						  | x <- [-size..(size - (target - 1))], y <- [-size..(size - (target - 1))]]
                       ++ [[(x,y),(x+1,y-1),(x+2,y-2)] 
                       	  | x <- [-size..(size - (target - 1))], y <- [-size..(size - (target - 1))]]

-- Calls the isWin helper function.
win :: [[Position]] -> Board -> Colour -> Bool
win winList board colour = isWin (colourFilter board colour) winList

-- Recursively iterate through lists, checking for a pattern match.
isWin :: [Position] -> [[Position]] -> Bool
isWin positions (pattern:patterns) = (isFound pattern positions) || (isWin positions patterns)
isWin [] _ = False
isWin _ [] = False

isFound :: Eq a => [a] -> [a] -> Bool
isFound (x:xs) ys = (x `elem` ys) && (isFound xs ys)
isFound [] _ = True

-- Filters out the positions of the given colour.
colourFilter :: Board -> Colour -> [Position]
colourFilter board colour = map fst $ filter (\(_, col) -> col == colour) (pieces board)

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
          halfgridwidth = (size $ board world) `quot` 2

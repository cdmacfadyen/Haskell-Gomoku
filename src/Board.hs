module Board where

import Control.Monad
import Debug.Trace

-- | Player piece colour.
data Colour = Black | White
  deriving (Read, Show, Eq)

transformUp = (0, 1)
transformUpRight = (1, 1)
transformRight = (1, 0)
transformDownRight = (1, -1)
transformDown = (0, -1)
transformDownLeft = (-1, -1)
transformLeft = (-1, 0)
transformUpLeft = (-1, -1)

-- Returns a list containing all of the transform directions.
allTransforms :: [Position]
allTransforms = [transformUp, transformUpRight, transformRight, transformDownRight, transformDown, transformDownLeft, transformLeft, transformUpLeft]

-- Given a position and a transform, return the position modified by the transform
transform :: Position -> Position -> Position
transform (x, y) (x_diff, y_diff) = (x + x_diff, y + y_diff)

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
                     mouse_board :: Maybe Position,
                     hint :: Maybe Position,
                     pieces :: [Piece], -- ^ Position List.
                   	 won :: Maybe Colour } -- ^ Win Status.
         deriving (Read, Show)

data World = World { board :: Board, -- ^ Board Representation
                     turn :: Colour,
                     mouse :: (Float,Float),
                     human :: Colour,
                     computer :: Colour,
                     settings :: Settings,
             		     width :: Int } -- ^ Width
         deriving (Read, Show)

data Settings = Settings { grid_size :: Int, -- ^ Board Representation
                     	   target_size :: Int,
                     	   ai_difficulty :: Int,
                     	   human_colour :: Maybe Colour,
                     	   configured :: Bool,
                         game_in_progress :: Bool} -- ^ Width
         deriving (Read, Show)


-- Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> Colour -> Position -> Maybe Board
makeMove b col p = if contains p $ pieces b then Nothing else Just newboard {won = checkWon newboard, hint = Nothing}
        where newboard = b {pieces = ((p, col) : (pieces b))}

-- Given the same arguments as makeMove, return a new board if the move was successful, or the original board if it was not.
maybeMakeMove :: Board -> Colour -> Position -> Board
maybeMakeMove b col p = case makeMove b col p of
                             Just board -> board
                             Nothing    -> b

-- Checks if there is a piece of either colour at the given position
contains :: Position -> [Piece] -> Bool
contains position positions = (position, White) `elem` positions || (position, Black) `elem` positions

-- Gets a World from original World and Maybe Board
-- Have to check if Maybe Board is Nothing or Board before returning World
maybeBoardToWorld :: World -> Maybe Board -> World
maybeBoardToWorld b Nothing = b
maybeBoardToWorld b (Just mBoard) = b {board = mBoard, turn = switch (turn b), settings = new_settings}
          where new_settings = (settings b){game_in_progress = (is_in_progress_game b)}

is_in_progress_game :: World -> Bool
is_in_progress_game w = case (won (board w)) of
        Just Black -> False
        Just White -> False
        Nothing -> True

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> Maybe Colour
checkWon b = checkn b (target b) 

-- Convenience function for getting the position of the given piece
getPiecePos :: Piece -> Position
getPiecePos (position, _) = position

-- Given the list of pieces, a transform direction to check, a position of a piece to check, the target number in a row and a colour to check victory for, return Just colour if colour has won, or Nothing otherwise.
checkTransformColour :: [Piece] -> Position -> Position -> Int -> Colour -> Maybe Colour
checkTransformColour piecelist (x_diff, y_diff) (x, y) targetN colour
    | ((x, y), colour) `elem` piecelist = if targetN == 0
                                                then Nothing
                                                else checkTransformColour piecelist (x_diff, y_diff) (x + x_diff, y + y_diff) (targetN - 1) colour
    | otherwise                         = if targetN == 0
                                                then Just colour
                                                else Nothing

checkTransformColourCount :: [Piece] -> Position -> Position -> Int -> Colour -> Int
checkTransformColourCount piecelist (x_diff, y_diff) (x, y) targetN colour
    | ((x, y), colour) `elem` piecelist = if targetN == 0
                                                then 0
                                                else checkTransformColourCount piecelist (x_diff, y_diff) (x + x_diff, y + y_diff) (targetN - 1) colour
    | otherwise                         = if targetN == 0
                                                then 1
                                                else 0

-- Filters out the positions of the given colour.
colourFilter :: Board -> Colour -> [Position]
colourFilter board colour = map fst $ filter (\(_, col) -> col == colour) (pieces board)

---- An evaluation function for a minimax search. Given a board and a colour
---- return an integer indicating how good the board is for that colour.
--evaluate :: Board -> Colour -> Int
--evaluate board colour = case checkWon board of {(Just White) -> 1; (Just Black) -> -1; (_) -> 0}

-- Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkn :: Board -> Int -> Maybe Colour
checkn b targetN = msum [(checkTransformColour piecelist transform position targetN colour)
                    | (position, colour) <- piecelist,
                      transform <- allTransforms,
                      onedge position transform] -- Only check pieces that are on the edges of a row/column/diagonal at the top level, middle spaces will be checked by recursion from these, and this ensures you can't have more than targetN in a row.
    where piecelist = pieces b
          onedge (x, y) (x_diff, y_diff) = not (contains (x - x_diff, y - y_diff) piecelist)

-- Takes a board, a number in a row to check for and a colour to check for, return the number of lines of exactly that length belonging to that player
countNConnected :: Board -> Int -> Colour -> Int
countNConnected b targetN col = sum [checkTransformColourCount piecelist transform position targetN col
                                    |(position, colour) <- piecelist,
                                     transform <- allTransforms]
    where piecelist = pieces b

squareSize :: World -> Float
squareSize w = fromIntegral (width w) / (fromIntegral . size $ board w)

-- Given a world and a screen-space co-ordinate, return the closest board position to that co-ordinate if it is in bounds, or Nothing if it is not.
screenSpaceToBoardSpace :: World -> (Float, Float) -> Maybe Position
screenSpaceToBoardSpace world (screenx, screeny) 
    | boardx >= minbound && boardx <= maxbound && boardy >= minbound && boardy <= maxbound = Just (boardx, boardy)
    | otherwise = Nothing
    where even = (size $ board world) `rem` 2 == 0
          gridsize      = squareSize world
          boardx        = if even
                            then round $ screenx / gridsize
                          else round $ (screenx - gridsize / 2) / gridsize
          boardy        = if even
                            then round $ screeny / gridsize
                          else round $ (screeny - gridsize / 2) / gridsize
          halfgridwidth = (size $ board world) `quot` 2
          minbound      = if even
                            then -halfgridwidth
                          else -halfgridwidth - 1
          maxbound      = halfgridwidth

-- Given a world and a board-space co-ordinate position, return the screen-space co-ordinate of that position.
boardSpaceToScreenSpace :: World -> Position -> (Float, Float)
boardSpaceToScreenSpace world (boardx, boardy)
    | even      = (screenx, screeny)
    | otherwise = (screenx + gridsize / 2, screeny + gridsize / 2)
    where even = (size $ board world) `rem` 2 == 0
          gridsize = squareSize world
          screenx = gridsize * fromIntegral boardx
          screeny = gridsize * fromIntegral boardy

is_even :: Board -> Bool
is_even board = (size board) `rem` 2 == 0

get_min :: Board -> Int
get_min board = if is_even board then -halfgridwidth else -halfgridwidth - 1
    where halfgridwidth = (size board) `quot` 2

get_max :: Board -> Int
get_max board = (size board) `quot` 2

validMove :: Board -> Position -> Bool
validMove board (x, y) = x >= (get_min board) && y >= (get_min board) && x <= (get_max board) && y <= (get_max board)

-- Undoes the supplied number of moves
undo :: Int -> World -> World
undo n w = foldr (.) id (replicate n undoOne) w

-- Undoes a single move
undoOne :: World -> World
undoOne w = do let (newmoves, newturn) = if length currentmoves > 0
                                            then (tail currentmoves, switch currentturn)
                                            else (currentmoves, currentturn)
               let newboard = currentboard {pieces = newmoves}
               w {board = newboard, turn = newturn}
    where currentboard = board w
          currentturn  = turn w
          currentmoves = pieces currentboard

settings_complete :: Settings -> Bool
settings_complete s = if ((grid_size s) /= 0) && 
                         ((target_size s) /= 0) && 
                         ((ai_difficulty s) /= 0) && 
                         ((human_colour s) /= Nothing) 
                         then True
                         else False

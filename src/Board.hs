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


-- | Returns a list containing all of the transform directions.
allTransforms :: [Position]
allTransforms = [transformUp, transformUpRight, transformRight, 
                   transformDownRight, transformDown, transformDownLeft, 
                      transformLeft, transformUpLeft]

-- | Given a position and a transform, return the position modified by the transform.
transform :: Position -> -- ^Takes a position to apply transformation to
             Position -> -- ^Transformation to apply on postion
             Position -- ^Returns the transformed postion
transform (x, y) (x_diff, y_diff) = (x + x_diff, y + y_diff)

-- | Switch current player.
switch :: Colour -> -- ^Takes the colour of the current player
          Colour -- ^Returns the opposite colour to change turns
switch Black = White
switch White = Black

-- ^Indicate a piece position of board.
type Position = (Int, Int)

-- ^Indicate a piece position of board.
type Piece = (Position, Colour)

-- | Game Board with its attributes
data Board = Board { size :: Int, -- ^Board Size
                     target :: Int, -- ^Target 'in-a-row'
                     mouse_board :: Maybe Position, -- ^Mouse position 
                                                    -- on the Board
                     hint :: Maybe Position, -- ^Hint Position
                     pieces :: [Piece], -- ^Position List
                   	 won :: Maybe Colour } -- ^Win Status
         deriving (Read, Show)

-- | World state acting as the whole game by containing the game Board along with 
-- details of settings
data World = World { board :: Board, -- ^Board Representation
                     turn :: Colour, -- ^Current Player Colour
                     mouse :: (Float,Float), -- ^Mouse position on the screen
                     human :: Colour, -- ^Colour Of The Human Player
                     computer :: Colour, -- ^Colour Of The AI Player
                     settings :: Settings, -- ^Settings Loaded In
             		     width :: Int } -- ^Width Of The Board
         deriving (Read, Show)

data Settings = Settings { grid_size :: Int, -- ^Board Representation
                     	   target_size :: Int, -- ^Target 'in-a-row'
                     	   ai_difficulty :: Int, -- ^Level Of The AI Player
                     	   human_colour :: Maybe Colour, -- ^Colour Of Human Player
                     	   configured :: Bool, -- ^Stores whether configured or not
                         game_in_progress :: Bool} -- ^Width Of The Board
         deriving (Read, Show)


-- | Play a move on the board; return 'Nothing' if the move is invalid
-- (e.g. outside the range of the board, or there is a piece already there)
makeMove :: Board -> -- ^The current Board.
            Colour -> -- ^The colour of the current player
            Position -> -- ^The position a piece is trying to be placed
            Maybe Board -- ^The new board state after attempted move
makeMove b col p = if contains p $ pieces b then Nothing else 
                   Just newboard {won = checkWon newboard, hint = Nothing}
                      where newboard = b {pieces = ((p, col) : (pieces b))}

-- | Given the same arguments as makeMove, return a new board if the move was
-- successful, or the original board if it was not.
maybeMakeMove :: Board -> -- ^Takes the current board
                 Colour -> -- ^The current player's colour
                 Position -> -- ^The position where a piece is trying to be placed
                 Board -- ^Returns a new board state
maybeMakeMove b col p = case makeMove b col p of
                             Just board -> board
                             Nothing    -> b

-- | Checks if there is a piece of either colour at the given position
contains :: Position -> -- ^Takes the position of board where piece is trying to go
            [Piece] -> -- ^The list of all pieces on the board
            Bool -- ^Returns whether the position is occupied or not
contains position positions = (position, White) `elem` positions 
                              || (position, Black) `elem` positions

-- | Gets a World from original World and Maybe Board
-- Have to check if Maybe Board is Nothing or Board before returning World
maybeBoardToWorld :: World -> -- ^Takes the current World state
                     Maybe Board -> -- ^The new Board state to be updated in World
                     World -- ^A new World state with the new Board state
maybeBoardToWorld b Nothing = b
maybeBoardToWorld b (Just mBoard) = b {board = mBoard, turn = switch (turn b),
                                       settings = new_settings}
                                        where new_settings = (settings b) {game_in_progress = 
                                           (is_in_progress_game b)}

-- | Checks if game is still ongoing or not
is_in_progress_game :: World -> -- ^Takes current World state
                       Bool -- ^True or False depending on whether game is ongoing or not
is_in_progress_game w = case (won (board w)) of
        Just Black -> False
        Just White -> False
        Nothing -> if (configured (settings w)) == False then False else True

-- | Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkWon :: Board -> -- ^Takes current Board state
            Maybe Colour -- ^Rreturns winning player colour
checkWon b = checkn b (target b) 

-- | Convenience function for getting the position of the given piece
getPiecePos :: Piece -> -- ^Takes piece you want the position for
               Position -- ^Returns position of a piece
getPiecePos (position, _) = position

-- | Given the list of pieces, a transform direction to check, a position
-- of a piece to check, the target number in a row and a colour to check 
-- victory for, return Just colour if colour has won, or Nothing otherwise.
checkTransformColour :: [Piece] -> -- ^Takes list of all pieces on the board
                        Position -> -- ^The transform direction
                        Position -> -- ^Piece position to check
                        Int -> -- ^What the target 'in-a-row' is
                        Colour -> -- ^Colour to check if won
                        Maybe Colour -- ^Return the colour of player if they won
checkTransformColour piecelist (x_diff, y_diff) (x, y) targetN colour
    | ((x, y), colour) `elem` piecelist = if targetN == 0
                                                then Nothing
                                                else checkTransformColour piecelist 
                                                   (x_diff, y_diff) 
                                                      (x + x_diff, y + y_diff) 
                                                         (targetN - 1) colour
    | otherwise                         = if targetN == 0
                                                then Just colour
                                                else Nothing

-- | Checks one direction using transformed co-ordinate and returns number of 'n' length lines
checkTransformColourCount :: [Piece] -> -- ^Takes a list of pieces on the board
                             Position -> -- ^Position to apply transformation to
                             Position -> -- ^Transformation to apply to position
                             Int -> -- ^Target 'in-a-row' to match against
                             Colour -> -- ^Colour to check for
                             Int -- ^The number of lines matching the search
checkTransformColourCount piecelist (x_diff, y_diff) (x, y) targetN colour
    | ((x, y), colour) `elem` piecelist = if targetN == 0
                                                then 0
                                                else checkTransformColourCount piecelist 
                                                   (x_diff, y_diff) 
                                                      (x + x_diff, y + y_diff) 
                                                         (targetN - 1) colour
    | otherwise                         = if targetN == 0
                                                then 1
                                                else 0

-- | Filters out the positions of the given colour.
colourFilter :: Board -> -- ^Takes current Board state
                Colour -> -- ^A colour indicating player, either Black or White
                [Position] -- ^Returns a list of positions belonging to the given colour
colourFilter board colour = map fst $ filter (\(_, col) -> col == colour) (pieces board)

-- | Check whether the board is in a winning state for either player.
-- Returns 'Nothing' if neither player has won yet
-- Returns 'Just c' if the player 'c' has won
checkn :: Board -> -- ^Takes the current Board state
          Int -> -- ^The target 'in-a-row' needed for a win
          Maybe Colour -- ^Colour rreturn if won else nothing
checkn b targetN = msum [(checkTransformColour piecelist transform position targetN colour)
                    | (position, colour) <- piecelist,
                      transform <- allTransforms,
                      onedge position transform] -- ^Only check pieces that are on the edges 
                                                 -- of a row/column/diagonal at the top level, 
                                                 -- middle spaces will be checked by recursion from these, 
                                                 -- and this ensures you can't have more than targetN in a row.
    where piecelist = pieces b
          onedge (x, y) (x_diff, y_diff) = not (contains 
                                                  (x - x_diff, y - y_diff) 
                                                     piecelist)

-- | Takes a board, a number in a row to check for and a colour to check for, 
-- return the number of lines of exactly that length belonging to that player
countNConnected :: Board -> -- ^Takes the current Board state
                   Int -> -- ^How many rows of 'n' length you want to search for
                   Colour -> -- ^The colour (player) of the pieces you are 
                             -- searching for
                   Int -- ^Returns the number of lines that have been found 
                       -- matching the Int
countNConnected b targetN col = sum [checkTransformColourCount piecelist transform position targetN col
                                    |(position, colour) <- piecelist,
                                     transform <- allTransforms]
    where piecelist = pieces b

-- | Works out the number of pixels in a square on the grid
squareSize :: World -> -- ^Takes current World state
              Float -- ^Returns the number of pixels in a grid square
squareSize w = fromIntegral (width w) / (fromIntegral . size $ board w)

-- | Given a world and a screen-space co-ordinate, return the closest board position to 
-- that co-ordinate if it is in bounds, or Nothing if it is not.
screenSpaceToBoardSpace :: World -> -- ^Takes the current World state
                           (Float, Float) -> -- ^The position clicked on the board
                           Maybe Position -- ^Returns the nearest valid position on the board, if any
screenSpaceToBoardSpace world (screenx, screeny) 
    | boardx >= minbound && boardx <= maxbound && boardy >= minbound && 
         boardy <= maxbound = Just (boardx, boardy)
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

-- | Given a world and a board-space co-ordinate position, return the 
-- screen-space co-ordinate of that position.
boardSpaceToScreenSpace :: World -> -- ^Takes the current World state
                           Position -> -- ^A board position given
                           (Float, Float) -- ^The screen space co-ordinate of 
                                          -- the board position
boardSpaceToScreenSpace world (boardx, boardy)
    | even      = (screenx, screeny)
    | otherwise = (screenx + gridsize / 2, screeny + gridsize / 2)
    where even = (size $ board world) `rem` 2 == 0
          gridsize = squareSize world
          screenx = gridsize * fromIntegral boardx
          screeny = gridsize * fromIntegral boardy

-- | Checks if the Board is even or not
is_even :: Board -> -- ^Takes the current Board state
           Bool -- ^True if even, else False if odd
is_even board = (size board) `rem` 2 == 0

-- | Gets the minimum bound of the Board
get_min :: Board -> -- ^Takes the current Board state
           Int -- ^Returns minimum bound of Board
get_min board = if is_even board then -halfgridwidth else -halfgridwidth - 1
    where halfgridwidth = (size board) `quot` 2

-- | Gets the maximum bound of the Board
get_max :: Board -> -- ^Takes the current Board state
           Int -- ^Returns maximum bound of Board
get_max board = (size board) `quot` 2

-- | Checks to see if the position clicked maps to a valid board space
validMove :: Board -> -- ^Takes current Board state
             Position -> -- ^Position current clicked
             Bool -- ^True if valid, else False id invalid
validMove board (x, y) = x >= (get_min board) && y >= (get_min board) && 
                            x <= (get_max board) && y <= (get_max board)

-- | Undoes the supplied number of moves
undo :: Int -> -- ^How many moves you want to undo
        World -> -- ^Current World state
        World -- ^New World state with 'x' amount of moves undone
undo n w = foldr (.) id (replicate n undoOne) w

-- | Undoes a single move
undoOne :: World -> -- ^Takes current World
           World -- ^Returns new World proir to current World
undoOne w = do let (newmoves, newturn) = if length currentmoves > 0
                                            then (tail currentmoves, 
                                                     switch currentturn)
                                            else (currentmoves, currentturn)
               let newboard = currentboard {pieces = newmoves}
               w {board = newboard, turn = newturn}
    where currentboard = board w
          currentturn  = turn w
          currentmoves = pieces currentboard

-- | Checks if the settings have been initialised
settings_complete :: Settings -> -- ^Takes current Settings applied
                     Bool -- ^Returns a bool indicating Settings addition or not
settings_complete s = if ((grid_size s) /= 0) && 
                         ((target_size s) /= 0) && 
                         ((ai_difficulty s) /= 0) && 
                         ((human_colour s) /= Nothing) 
                         then True
                         else False

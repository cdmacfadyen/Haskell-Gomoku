module AdvancedAI where

import Board
import Debug.Trace
import Data.List
import Data.Ord

-- List comprehension to examine all positions on the board and
-- build a list of unused positions. Availability is checked by way
-- of the 'contains' method.
get_possible_moves :: Board -- ^ Current board.
                      -> Colour -- ^ Player to check for.
                      -> [Position] -- ^ List of positions.
get_possible_moves board colour = [ (x, y) 
                                  | x <- [min..max], 
                                    y <- [min..max], not $ contains (x, y) (pieces board)]
    where 
      min = get_min board -- Minimum board bound, depending on if even/odd.
      max = get_max board -- Maximum board bound, depending on if even/odd.

-- Generate a list of moves for the AI to consider for its next one. This can exclude some moves which the AI should not consider - for example ones far away from any current piece - in order to reduce the number of moves that have to be evaluated. Can also be ordered such that the moves at the start of the list are the ones most likely to be successful, as this will result in the AI being able to prune more effectively.
get_considered_moves :: Board -> Colour -> [Position]
get_considered_moves board colour = if null (pieces board) then [(0, 0)] else nub [doTrans pos trans | pos <- pieces board, trans <- (allTransforms ++ (map doubleTrans allTransforms)), unoccupied pos trans && validMove board (doTrans pos trans)]
    where doTrans pos trans = transform (getPiecePos pos) trans
          doubleTrans (t_x, t_y) = (2 * t_x, 2 * t_y)
          unoccupied pos trans = not (contains (doTrans pos trans) (pieces board))

infty :: Int
infty = maxBound :: Int

getbestmove :: Board -> Int -> Colour -> Position
getbestmove board depth colour = getpos (alphabeta board colour depth (-infty) infty colour)
    where getpos (pos, _) = pos

alphabeta :: Board -> Colour -> Int -> Int -> Int -> Colour -> (Position, Int)
alphabeta board current_turn depth alpha beta maximising_colour = if depth == 0 || null next_moves
                                                                     then (previousmove, evaluate board current_turn)
                                                                  else if current_turn == maximising_colour then
                                                                          maximise board next_moves current_turn depth (previousmove, -infty) alpha beta maximising_colour
                                                                       else minimise board next_moves current_turn depth (previousmove, infty) alpha beta maximising_colour
    where next_moves   = get_considered_moves board current_turn
          previousmove = getPiecePos (last (pieces board))

maximise :: Board -> [Position] -> Colour -> Int -> (Position, Int) -> Int -> Int -> Colour -> (Position, Int)
maximise board [] current_turn depth value alpha beta maximising_colour = value
maximise board (pos:poss) current_turn depth value alpha beta maximising_colour = do let newvalue = if doalphabeta > getval value
                                                                                                       then (pos, doalphabeta)
                                                                                                       else value
                                                                                     let newalpha = max alpha (getval newvalue)
                                                                                     if beta <= newalpha
                                                                                        then newvalue
                                                                                        else maximise board poss current_turn depth newvalue newalpha beta maximising_colour
    where doalphabeta = getval $ alphabeta (maybeMakeMove board current_turn pos) (switch current_turn) (depth - 1) alpha beta maximising_colour
          getval (_, val) = val

minimise :: Board -> [Position] -> Colour -> Int -> (Position, Int) -> Int -> Int -> Colour -> (Position, Int)
minimise board [] current_turn depth value alpha beta maximising_colour = value
minimise board (pos:poss) current_turn depth value alpha beta maximising_colour = do let newvalue = if doalphabeta < getval value
                                                                                                       then (pos, doalphabeta)
                                                                                                       else value
                                                                                     let newbeta = min beta (getval newvalue)
                                                                                     if newbeta <= alpha
                                                                                        then newvalue
                                                                                        else minimise board poss current_turn depth newvalue alpha newbeta maximising_colour
    where doalphabeta = getval $ alphabeta (maybeMakeMove board current_turn pos) (switch current_turn) (depth - 1) alpha beta maximising_colour
          getval (_, val) = val

-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Colour -> Int
evaluate b col =   (sum [(countNConnected b n col) * (weight n)| n <- [(target b), (target b) - 1 .. 1]]) `quot` 2  +
                   sum [(countNConnected b n (switch col)) * (-weight n)| n <- [(target b), (target b) - 1 .. 1]]
  where weight n = 2 ^ n

-- Makes an AI move, based on the best result from tree, and returns
-- a maybe board if successful.
move_ai :: World -> Board -> Colour -> Maybe Board
move_ai world board colour = makeMove board colour (getbestmove board 4 (turn world))

-- AI world, resulting from an AI move.
get_ai_world :: World -> World -- ^ New updated world.
get_ai_world world = maybeBoardToWorld world (move_ai world (board world) (turn world))

-- Update the world state after some time has passed.
updateWorld :: Float -- ^ Time since last update.
            -> World -- ^ Current world state.
            -> World -- ^ New world state.
updateWorld time world
  | turn world == (human world) = world -- This is the users turn.
  | otherwise       = case won (board world) of 
                      Nothing -> get_ai_world world
                      otherwise -> world

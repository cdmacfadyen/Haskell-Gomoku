module AdvancedAI where

import Board
import Debug.Trace
import Data.List
import Data.Ord

-- | List comprehension to examine all positions on the board and
-- build a list of unused positions. Availability is checked by way
-- of the 'contains' method.
get_possible_moves :: Board -> -- ^ Current 'Board'.
                      Colour -> -- ^ Player to check for.
                      [Position] -- ^ List of positions.
get_possible_moves board colour = [ (x, y) 
                                  | x <- [min..max], 
                                    y <- [min..max], not $ contains (x, y) 
                                            (pieces board)]
    where 
      min = get_min board -- ^Minimum board bound, depending on if even/odd.
      max = get_max board -- ^Maximum board bound, depending on if even/odd.

-- | Generate a list of moves for the AI to consider for its next one. 
-- This can exclude some moves which the AI should not consider - for example 
-- ones far away from any current piece - in order to reduce the number of moves 
-- that have to be evaluated. Can also be ordered such that the moves at the start 
-- of the list are the ones most likely to be successful, as this will result in 
-- the AI being able to prune more effectively.
get_considered_moves :: Board -> -- ^Takes current 'Board' state
                        Colour -> -- ^Player 'Colour' to check for
                        [Position] -- ^Returns list of positions for the given colour to consider
get_considered_moves board colour = if null (pieces board) then [(0, 0)] else nub 
                                       [doTrans pos trans | pos <- pieces board, trans <- 
                                          (allTransforms ++ (map doubleTrans allTransforms)), 
                                             unoccupied pos trans && validMove board 
                                                (doTrans pos trans)]
    where doTrans pos trans = transform (getPiecePos pos) trans
          doubleTrans (t_x, t_y) = (2 * t_x, 2 * t_y)
          unoccupied pos trans = not (contains (doTrans pos trans) (pieces board))

-- | Gets the largest Int for alpha-beta pruning, infinity.
infty :: Int
infty = maxBound :: Int

-- | Gets the best move for the AI for a specified depth to traverse the tree.
getbestmove :: Board -> -- ^Takes the current 'Board' state
               Int -> -- ^The depth to traverse down the tree
               Colour -> -- ^The colour to get the best move for
               Position -- ^The final 'best' position
getbestmove board depth colour = getpos (alphabeta board colour depth (-infty) infty colour)
    where getpos (pos, _) = pos

-- | Alpha-Beta Pruning algorithm that traverses the tree of 
-- possible 'Board' states
alphabeta :: Board -> -- ^Takes the current 'Board' state
             Colour -> -- ^To indicate the current turn
             Int -> -- ^How far down to search the tree
             Int -> -- ^The value of alpha
             Int -> -- ^The value of beta
             Colour -> -- ^The player that's to be maximised
             (Position, Int)
alphabeta board current_turn depth alpha beta maximising_colour 
    = if depth == 0 || null next_moves
         then (previousmove, evaluate board current_turn)
      else if current_turn == maximising_colour then
              maximise board next_moves current_turn depth 
                 (previousmove, -infty) alpha beta maximising_colour
           else minimise board next_moves current_turn depth 
                   (previousmove, infty) alpha beta maximising_colour
    where next_moves   = get_considered_moves board current_turn
          previousmove = getPiecePos (last (pieces board))

-- | Function that returns the best move tuple for a player based on the depth of 
-- traversal passed in
maximise :: Board -> -- ^Takes the current 'Board' state
            [Position] -> -- ^List of all possible moves
            Colour -> -- ^The current player's colour
            Int -> -- ^Depth to traverse for maximisation
            (Position, Int) -> -- ^Tuple containing details of the previous move
            Int -> -- ^The value of alpha
            Int -> -- ^The value of beta
            Colour -> -- ^Player that's to be maximised
            (Position, Int) -- ^The move with the maximum value
maximise board [] current_turn depth value alpha beta maximising_colour = value
maximise board (pos:poss) current_turn depth value alpha beta maximising_colour 
    = do let newvalue = if doalphabeta > getval value
                           then (pos, doalphabeta)
                           else value
         let newalpha = max alpha (getval newvalue)
         if beta <= newalpha
            then newvalue
            else maximise board poss current_turn depth newvalue newalpha beta maximising_colour
    where doalphabeta = getval $ 
             alphabeta (maybeMakeMove board current_turn pos) (switch current_turn) (depth - 1) 
                alpha beta maximising_colour
          getval (_, val) = val

-- | Function that returns the worst move tuple for a player based on the depth of 
-- traversal passed in
minimise :: Board -> -- ^Takes the current 'Board' state
            [Position] -> -- ^List of all possible moves
            Colour -> -- ^The current player's colour
            Int -> -- ^Depth to traverse for minimisation
            (Position, Int) -> -- ^Tuple containing details the previous move
            Int -> -- ^The value of alpha
            Int -> -- ^The value of beta
            Colour -> -- ^Player that's to be maximised
            (Position, Int) -- ^The move with the maximum value
minimise board [] current_turn depth value alpha beta maximising_colour = value
minimise board (pos:poss) current_turn depth value alpha beta maximising_colour 
    = do let newvalue = if doalphabeta < getval value
                           then (pos, doalphabeta)
                           else value
         let newbeta = min beta (getval newvalue)
         if newbeta <= alpha
            then newvalue
            else minimise board poss current_turn depth newvalue alpha newbeta maximising_colour
    where doalphabeta = getval $ 
             alphabeta (maybeMakeMove board current_turn pos) (switch current_turn) (depth - 1) 
                alpha beta maximising_colour
          getval (_, val) = val

-- | An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> -- ^Takes the current 'Board' state
            Colour -> -- ^Player colour that you want to evaluate the 'Board' for
            Int -- ^Returns a value based on how good the 'Board' is for the player
evaluate b col =   if goal > 3
                      then (sum [(countNConnected b n col) * (weight n)
                                | n <- [goal, goal - 1 .. 1]]) `quot` 2  +
                            sum [(countNConnected b n (switch col)) * (-weight n)
                                | n <- [goal, goal - 1 .. 1]]
                      else case checkWon b of
                              Just c -> if c == col then 100 else -100
                              Nothing -> 0
  where weight n = 2 ^ n
        goal = target b

-- | Makes an AI move, based on the best result from tree, and returns
-- a maybe board if successful.
move_ai :: World -> -- ^Takes the current 'World' state
           Board -> -- ^The current 'Board' state
           Colour -> -- ^The colour of the AI
           Maybe Board -- ^The new 'Board' state with the AI move, or Nothing
move_ai world board colour = makeMove board colour (getbestmove board depth (turn world))
    where difficulty = ai_difficulty (settings world)
          depth = 2 + difficulty

-- | AI world, resulting from an AI move.
get_ai_world :: World -> World -- ^ New updated 'World'.
get_ai_world world = maybeBoardToWorld world (move_ai world (board world) (turn world))

-- | Update the world state after some time has passed.
updateWorld :: Float -- ^Time since last update.
            -> World -- ^Current 'World' state.
            -> World -- ^New 'World' state.
updateWorld time world
  | turn world == (human world) = world -- This is the users turn.
  | otherwise       = case won (board world) of 
                      Nothing -> get_ai_world world
                      otherwise -> world

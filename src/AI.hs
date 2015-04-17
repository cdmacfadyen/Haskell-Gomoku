module AI where

import Board
import Debug.Trace
import Data.List
import Data.Ord

{-data GameTree = GameTree { game_board :: Board, -- ^ Board.
                           game_turn :: Colour, -- ^ Player turn.
                           next_moves :: [(Position, GameTree)] } -- ^ List of (Position, Tree).

build_tree:: (Board -> Colour -> [Position]) -- ^ Function to generate possible moves.
            -> Board -- ^ Board to represent state.
            -> Colour -- ^ Next player.
            -> GameTree -- ^ The generated tree.
build_tree gen_fun board colour = let moves = gen_fun board colour in -- Generated moves.
                        GameTree board colour (make_states moves)
  where
    make_states :: [Position] -> [(Position, GameTree)]
    make_states [] = []
    make_states (pos : positions)
        = case makeMove board colour pos of -- Try making the suggested move.
               Nothing -> make_states positions -- Not successful, no new state.
               -- Success, make the move and build the tree for the opposite player from here.
               Just board' -> (pos, build_tree gen_fun board' (switch colour)) : make_states positions
-}
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

-- This function gets the best move from an infinite game tree.
-- It should traverse up to a certain depth, and pick the move
-- which leads to the position with the best score for the player
-- whose turn it is at the top of the tree.
{--get_best_move :: Int -- ^ Maximum search depth.
               -> GameTree -- ^ Initial game tree.
               -> Position -- ^ Best position.
get_best_move depth tree = trace (show (game_turn tree) ++ " : " ++ (show choice)) choice
          where get_position (_, pos) = pos
                choice = get_position (alphabeta tree depth -1/0 1/0 (game_turn tree))
--}

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
    where next_moves   = get_possible_moves board current_turn
          previousmove = getPiecePos (last (pieces board))

maximise :: Board -> [Position] -> Colour -> Int -> (Position, Int) -> Int -> Int -> Colour -> (Position, Int)
maximise board [] current_turn depth value alpha beta maximising_colour = value
maximise board (pos:poss) current_turn depth value alpha beta maximising_colour = do let newvalue = if doalphabeta > (getval value)
                                                                                                       then (pos, doalphabeta)
                                                                                                       else value
                                                                                     let newalpha = min alpha (getval newvalue)
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

{-alphabetaRecurse :: Board    -> -- ^ The current board state
             Int      -> -- ^ The (maximum) depth of the game tree to traverse
             Int      -> -- ^ The current alpha value
             Int      -> -- ^ The current beta value
             Colour   -> -- ^ The colour of the player to maximise for
             Position -- ^ The move for the AI to play.
alphabetaRecurse tree depth alpha beta colour = if depth == 0 || length (next_moves tree) == 0
                                            then (evaluate (game_board tree) (game_turn tree), 
                                         else if colour == (game_turn tree) then
                                                 maximise (next_moves tree) depth -1/0 alpha beta colour
                                              else minimise (next_moves tree) depth 1/0 alpha beta colour

maximise :: Board     ->
            Int       ->
            Int       ->
            Int       ->
            Colour    ->
            Position

maximise board depth value alpha beta colour  = case children of 
                                    ((_, child):_) -> if beta <= alpha || null children
                                                         then (value, child)
                                                         else do let newValue = max value (alphabeta child (depth - 1) alpha beta colour)
                                                                 let newAlpha = max alpha newValue
                                                                 maximise child depth alpha beta colour
                                    otherwise      -> value

minimise :: Board     ->
            Int       ->
            Int       ->
            Int       ->
            Colour    ->
            Position

minimise GameTree {next_moves=children} depth value alpha beta colour  = case children of 
                                    ((_, child):_) -> if beta <= alpha || null children
                                                         then (value, child)
                                                         else do let newValue = min value (alphabeta child (depth - 1) alpha beta colour)
                                                                 let newAlpha = min alpha newValue
                                                                 minimise child depth alpha beta colour
                                    otherwise      -> value

-}
-- An evaluation function for a minimax search. Given a board and a colour
-- return an integer indicating how good the board is for that colour.
evaluate :: Board -> Colour -> Int
evaluate b col = sum [(countNConnected b n col) * (weight n)| n <- [(size b), (size b) - 1 .. 2]] +
                 sum [(countNConnected b n (switch col)) * (-weight n)| n <- [(size b), (size b) - 1 .. 2]]
  where weight n = 2 ^ n

{-
heuristic :: Int -> Colour -> [(Position, GameTree)] -> (Position, GameTree)
heuristic  depth _ [pos] = pos
heuristic  depth colour ps = trace (show values ++ show (map fst ps)) choice
   where values = map (minimax_search depth colour False . snd) ps
         choice = fst $ maximumBy (\x y -> compare (snd x) (snd y)) $ zip ps $ map (minimax_search depth colour False . snd) ps


minimax_search :: Int -> Colour -> Bool -> GameTree -> Int
minimax_search depth colour maxPlayer (GameTree board _ []) = eval
                        where eval = evaluate board colour
minimax_search 0 colour maxPlayer (GameTree board _ _) = eval
                        where eval = evaluate board colour
minimax_search depth colour maxPlayer (GameTree board game_turn moves)
       | checkWon board == Just colour = 100
       | checkWon board == Just (switch colour) = -100
       | maxPlayer     = maximum $ max
       | otherwise     = minimum $ min
          where nextTrees = map treeOf moves
                treeOf (p,tree) = tree
                max = (map (minimax_search (depth-1) colour False) nextTrees)
                min = (map (minimax_search (depth-1) colour True) nextTrees)
-}


-- Makes an AI move, based on the best result from tree, and returns
-- a maybe board if successful.
move_ai :: World -> Board -> Colour -> Maybe Board
move_ai world board colour = makeMove board colour (getbestmove board 3 (turn world))

-- AI world, resulting from an AI move.
get_ai_world :: World -> World -- ^ New updated world.
get_ai_world world = maybeBoardToWorld world (move_ai world (board world) (turn world))

-- Update the world state after some time has passed.
updateWorld :: Float -- ^ Time since last update.
            -> World -- ^ Current world state.
            -> World -- ^ New world state.
updateWorld time world
  | turn world == Black = world -- This is the users turn.
  | otherwise       = case won (board world) of 
                      Nothing -> get_ai_world world
                      otherwise -> world

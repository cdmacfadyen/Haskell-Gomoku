module AI where

import Board

type GameState = (Colour,Board)

data GameTree = GameTree { game_state :: GameState,
                           next_moves :: [(Position, GameTree)] } -- ^ List of (Position, Tree).

build_tree:: (Board -> Colour -> [Position]) -- ^ Function to generate possible moves.
            -> Board -- ^ Board to represent state.
            -> Colour -- ^ Next player.
            -> GameTree -- ^ The generated tree.
build_tree gen_fun board colour = let moves = gen_fun board colour in -- Generated moves.
                        GameTree (colour,board) (make_states moves)
  where
    make_states :: [Position] -> [(Position, GameTree)]
    make_states [] = []
    make_states (pos : positions)
        = case makeMove board colour pos of -- Try making the suggested move.
               Nothing -> make_states positions -- Not successful, no new state.
               -- Success, make the move and build the tree for the opposite player from here.
               Just board' -> (pos, build_tree gen_fun board' (switch colour)) : make_states positions

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

evaluate_state :: GameState -> Int
evaluate_state state = evaluate_board state

-- minmax algorithm, computes value of best outcome
minimax_search :: GameTree -> Int
minimax_search (GameTree state []) = evaluate_state state
minimax_search (GameTree (White,_) moves) = maximum (map minimax_search (snd moves))
minimax_search (GameTree (Black,_) moves) = minimum (map minimax_search (snd moves))

-- from a current world to a state
do_move :: World -> GameState
do_move world = case (build_tree (get_possible_moves (board world) (turn world))) of
                        GameTree state [] -> state
                        GameTree state@(col, _) moves -> snd (find_best col (comp col) (map (\x->(minimax_search x, state x)) (snd moves)))
    where comp White = (>)
          comp Black = (<)

find_best :: Colour -> (Int -> Int -> Bool) -> [(Int, GameState)] -> (Int, GameState)
find_best _ _ [x] = x
find_best f cmp ((x1,y1):xs) | winning_state f y1 = (x1,y1)
                             | otherwise = let (x2, y2) = find_best f cmp xs in
                                             if cmp x1 x2 then (x1,y1) else (x2,y2)
threshold = 1::Int

evaluate_board :: GameState -> Int
evaluate_board (col,board) = if winning_state col
                                then 1
                                else
                                  if winning_state col /= True
                                    then -1
                                    else 0

winning_state :: Colour -> GameState -> Bool
winning_state White st = evaluate_state st > threshold
winning_state Black st = evaluate_state st < -threshold


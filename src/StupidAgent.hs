module AI where

import Board

data GameTree = GameTree { game_board :: Board, -- ^ Board.
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
get_best_move :: Int -- ^ Maximum search depth.
               -> GameTree -- ^ Initial game tree.
               -> Position -- ^ Best position.
get_best_move depth tree = fst (head (next_moves tree))

-- Makes an AI move, based on the best result from tree, and returns
-- a maybe board if successful.
move_ai :: Board -> Colour -> GameTree -> Maybe Board
move_ai board colour tree = makeMove board colour (get_best_move 0 tree)

-- AI world, resulting from an AI move.
get_ai_world :: World -- ^ AI world.
                -> GameTree -- ^ Tree
                -> World -- ^ New updated world.
get_ai_world world tree = maybeBoardToWorld world (move_ai (board world) (turn world) tree)

-- Update the world state after some time has passed.
updateWorld :: Float -- ^ Time since last update.
            -> World -- ^ Current world state.
            -> World -- ^ New world state.
updateWorld time world
	| turn world == Black = world -- This is the users turn.
	| otherwise       = get_ai_world world (build_tree get_possible_moves (board world) (turn world))
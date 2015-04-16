module AI where

import Board
import Data.Maybe (isJust)

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

player_turn :: GameTree -> Bool
player_turn tree = (game_turn tree) == White

terminal :: GameTree -> Bool -- ^ A game is terminal (finished) either if one of the players has won, 
-- or if there are no moves available. It is defined by default using checkWin and moves.
terminal tree = isJust (checkWon (game_board tree)) || null (next_moves tree) 

minimax :: GameTree -> Int
minimax tree | (player_turn tree) = maxValue tree (-2,2) 
             | otherwise    = minValue tree (-2,2)
    where maxValue tree (a,b) | terminal tree = eval tree
                           | otherwise = snd $ foldr (\g' (a',v) -> 
                                       let newV = max v (minValue g' (a',b)) in
                                       if v >= b 
                                       then (a',v) -- If a state with an evaluation higher than beta has been found, we don't bother checking any further nodes on this branch.
                                       else (max a' newV, newV)) -- If not, we calculate the new alpha and v values.
                                (a,-2) (flattenTuple (next_moves tree))
          minValue g (a,b) | terminal tree = eval tree
                           | otherwise = snd $ foldr (\g' (b',v) ->
                                       let newV = min v (maxValue g' (a,b')) in
                                       if v <= a
                                       then (b',v) -- If a state with an evaluation lower than alpha has been found, ignore the rest of the nodes on this branch.
                                       else (min b' newV, newV)) -- If not, calculate the new beta and v values.
                                (b,2) (flattenTuple (next_moves tree))

flattenTuple :: [(Position,GameTree)] -> [GameTree]
flattenTuple [] = []
flattenTuple ((pos,tree):xs) = tree : flattenTuple xs

eval :: GameTree -> Int
eval tree = case (checkWon (game_board tree)) of
          (Just White) -> 1
          (Just Black) -> -1
          (_) -> 0

-- AI world, resulting from an AI move.
get_ai_world :: World -- ^ AI world.
                -> GameTree -- ^ Tree
                -> World -- ^ New updated world.
get_ai_world world tree = fst $ head $ sortBy f $ map (\g -> (world, minimax tree)) (flattenTuple (next_moves tree)) where f (_,x) (_,y) | (turn world) == player_turn = compare x y | otherwise = invertCompare x y
                        invertCompare x y | x == y = EQ
                          | x >= y = LT
                          | otherwise = GT

-- Update the world state after some time has passed.
updateWorld :: Float -- ^ Time since last update.
            -> World -- ^ Current world state.
            -> World -- ^ New world state.
updateWorld time world
  | turn world == Black = world -- This is the users turn.
  | otherwise       = get_ai_world world (build_tree (get_possible_moves (board world) (turn world))
 














--compMove :: Game a => a -> Player -> a
--compMove game player = fst $ head $ sortBy f $ map (\g -> (g, minimax g)) (nextStates game)
--    where f (_,x) (_,y) | player == playerOne = compare x y
--      | otherwise = invertCompare x y
--    invertCompare x y | x == y = EQ
--          | x >= y = LT
--          | otherwise = GT



















---- This function gets the best move from an infinite game tree.
---- It should traverse up to a certain depth, and pick the move
---- which leads to the position with the best score for the player
---- whose turn it is at the top of the tree.
--get_best_move :: Int -- ^ Maximum search depth.
--               -> GameTree -- ^ Initial game tree.
--               -> Position -- ^ Best position.
--get_best_move depth tree = fst (head (next_moves tree))

---- Makes an AI move, based on the best result from tree, and returns
---- a maybe board if successful.
--move_ai :: Board -> Colour -> GameTree -> Maybe Board
--move_ai board colour tree = makeMove board colour (get_best_move 0 tree)

---- AI world, resulting from an AI move.
--get_ai_world :: World -- ^ AI world.
--                -> GameTree -- ^ Tree
--                -> World -- ^ New updated world.
--get_ai_world world tree = maybeBoardToWorld world (move_ai (board world) (turn world) tree)








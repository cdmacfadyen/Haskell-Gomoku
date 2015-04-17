module Main where

import System.Environment
import Data.List
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Board
import Draw
import Input
import AI

-- | Initialize the main IO loop.
main :: IO ()		-- ^ IO State.
-- | Starts up a graphics window and sets up handlers for 
-- dealing with inputs and updating the world state.
main = do args <- getArgs
          background <- loadBMP "images/background.bmp"
          black_piece <- loadBMP "images/black.bmp"
          white_piece <- loadBMP "images/white.bmp"
          undo_btn <- loadBMP "images/undo.bmp"
          save_btn <- loadBMP "images/save.bmp"
          undo_btn_h <- loadBMP "images/undo-highlight.bmp"
          save_btn_h <- loadBMP "images/save-highlight.bmp"
          restart_btn <- loadBMP "images/restart.bmp" 
          restart_btn_h <- loadBMP "images/restart_h.bmp"
          thinking <- loadBMP "images/thinking.bmp"
          
          world <- if length args == 4
          			  then return $ initialise_world args
          			  else 
          			  	if (args !! 0) == "def"
          			  		then return $ default_world
          			  		else 
          			  			if (isInfixOf ".save" (args !! 0))
          			  				then loadGame $ args !! 0
	          			  			else 
	          			  				if (args !! 0) == "usage"
	          			  					then error print_usage
	          			  					else error print_usage

          -- Keeping draw and update world 'pure' functions which are then converted to IO equivalents only in this function, 
          -- since they don't do any IO actions themselves.
          playIO (InWindow "Gomoku" (1100, 1100) (10, 10)) (greyN 0.3) 10
                 world
                 --(\x -> drawWorld x background)
                 (return . (\x -> drawWorld x background black_piece white_piece undo_btn 
                 					save_btn undo_btn_h save_btn_h restart_btn restart_btn_h
                 					thinking)) -- Convert the world state to gloss state.
                 -- | Called if there is an input event. If it is the
           	      --human player's turn, should update the board.
                 handleInput -- handleInput is an impure function since it saves/loads files.
                 -- | Called 10 times per second. If it is AI's turn,
                 -- should update the board with an AI generated move.
                 (curry $ return . (uncurry updateWorld))

loadGame :: String -> IO World
loadGame filename = do putStrLn $ "Loading game from file: " ++ show filename
                       contents <- readFile filename
                       return (read contents :: World)

-- | Default board: 6x6, target is 3 in a row, no initial pieces
default_board = Board 6 3 Nothing Nothing [] Nothing

-- | Default world: initial board, black is current player.
default_world = World default_board Black (0,0) Black White 600

-- Initialise board from command line arguments.
initialise_board :: Int -> Int -> Board
initialise_board board_size target = Board (check_size board_size) (check_target target) Nothing Nothing [] Nothing

-- Initialise world from command line arguments.
initialise_world :: [String] -> World
initialise_world args = World (initialise_board (read(args !! 1)::Int) (read(args !! 2)::Int)) -- Init board
								Black -- First player, hardcoded, must always be black.
								(0,0) -- Initial mouse position default
								(colour) -- Human player colour
								(switch colour) -- Computer colour
								600 -- Width, hard-coded must not be changed ever.
			where colour = get_colour_from_command (args !! 3)

check_size :: Int -> Int
check_size size = if 3 <= size && size <= 19 then size else error "Incorrect board size, try again [3-19]"

check_target :: Int -> Int
check_target target = if 3 <= target && target <= 6 then target else error "Incorrect target size, try again [3-6]"

get_colour_from_command :: String -> Colour
get_colour_from_command command
				| command == "Black" = Black
				| command == "White" = White
				|otherwise			 = error "Incorrect colour input, try again! [Black/White]"

print_usage :: String
print_usage = "usage: gomoku world_type board_size target_size which_colour\
				\\n\t\t world_type: string argument to indicate if new game, load game\
				\ or default settings [new || <name_of_load file> || def]\
				\\n\t\t board_size: allowed 3 <= size <= 19\
				\\n\t\t target_size: allowed 3 <= target <= 16\
				\\n\t\t which_colour: string arguments [Black || White]\
				\\n Please note that if you are using the default setting, do not pass\
				\ any other parameters, other than the string def."

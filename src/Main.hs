module Main where

import System.Environment
import Data.List
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss
import Board
import Draw
import Input
import AdvancedAI

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
          undo_btn_h <- loadBMP "images/undo-h.bmp"
          save_btn_h <- loadBMP "images/save-h.bmp"
          restart_btn <- loadBMP "images/restart.bmp" 
          restart_btn_h <- loadBMP "images/restart-h.bmp"
          thinking <- loadBMP "images/thinking.bmp"

          ai_difficulty <- loadBMP "images/in_game_settings/ai_difficulty.bmp"
          black_button <- loadBMP "images/in_game_settings/black.bmp"
          done <- loadBMP "images/in_game_settings/done.bmp"
          done_h <- loadBMP "images/done-h.bmp"
          grid_size <- loadBMP "images/in_game_settings/grid_size.bmp"
          nineteen <- loadBMP "images/in_game_settings/nineteen.bmp"
          six <- loadBMP "images/in_game_settings/six.bmp"
          target_size <- loadBMP "images/in_game_settings/target_size.bmp"
          three <- loadBMP "images/in_game_settings/three.bmp"
          white_button <- loadBMP "images/in_game_settings/white.bmp"
          colour_button <- loadBMP "images/in_game_settings/your_colour.bmp"

          black_won <- loadBMP "images/black_won.bmp"
          white_won <- loadBMP "images/white_won.bmp"
          hint_button <- loadBMP "images/hint.bmp"
          hint_button_h <- loadBMP "images/hint-h.bmp"

          black_h <- loadBMP "images/in_game_settings/black-h.bmp"
          white_h <- loadBMP "images/in_game_settings/white-h.bmp"
          three_h <- loadBMP "images/in_game_settings/three-h.bmp"
          six_h <- loadBMP "images/in_game_settings/six-h.bmp"
          nineteen2 <- loadBMP "images/in_game_settings/nineteen.bmp"
          nineteen_h <- loadBMP "images/in_game_settings/nineteen-h.bmp"
          easy <- loadBMP "images/in_game_settings/easy.bmp"
          easy_h <- loadBMP "images/in_game_settings/easy-h.bmp"
          hard <- loadBMP "images/in_game_settings/hard.bmp"
          hard_h <- loadBMP "images/in_game_settings/hard-h.bmp"

          med <- loadBMP "images/in_game_settings/med.bmp"
          med_h <- loadBMP "images/in_game_settings/med-h.bmp"
          tied <- loadBMP "images/tied.bmp"
          
          world <- if length args == 4
          			  then return $ initialise_world args
          			  else 
          			  	if length args == 0
                      then error print_usage
                      else 
                        if (args !! 0) == "def"
              			  		then return $ default_world
              			  		else 
              			  			if (isInfixOf ".save" (args !! 0))
              			  				then loadGame $ args !! 0
    	          			  			else 
    	          			  				if (args !! 0) == "usage"
    	          			  					then error print_usage
    	          			  					else 
    	          			  						if (args !! 0) == "ingame"
    	          			  							then return $ default_world_ingame_sets
    	          			  							else error print_usage

          -- Keeping draw and update world 'pure' functions which are then converted to IO equivalents only in this function, 
          -- since they don't do any IO actions themselves.
          playIO (InWindow "Gomoku" (1200, 750) (10, 10)) (greyN 0.3) 10
                 world
                 --(\x -> drawWorld x background)
                 (return . (\x -> drawWorld x background black_piece white_piece undo_btn 
                 					save_btn undo_btn_h save_btn_h restart_btn restart_btn_h
                 					thinking ai_difficulty black_button done grid_size
                 					nineteen six target_size three white_button colour_button black_won white_won hint_button
                          hint_button_h done_h black_h white_h three_h six_h nineteen2 nineteen_h easy easy_h hard 
                          hard_h med med_h tied)) -- Convert the world state to gloss state.
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
default_world = World default_board Black (0,0) Black White (configure_settings(default_settings)) 600

default_world_ingame_sets = World default_board Black (0,0) Black White default_settings_in_game 600

default_settings = Settings 0 0 0 Nothing True True

default_settings_in_game = Settings 0 0 0 Nothing False False

configure_settings :: Settings -> Settings
configure_settings settings = settings{configured=True}

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
								default_settings -- settings configured in command line.
								600 -- Width, hard-coded must not be changed ever.
			where colour = get_colour_from_command (args !! 3)

check_size :: Int -> Int
check_size size = if 2 <= size && size <= 19 then size else error "Incorrect board size, try again [2-19]"

check_target :: Int -> Int
check_target target = if 3 <= target && target <= 6 then target else error "Incorrect target size, try again [3-6]"

get_colour_from_command :: String -> Colour
get_colour_from_command command
				| command == "Black" = Black
				| command == "White" = White
				|otherwise			 = error "Incorrect colour input, try again! [Black/White]"

print_usage :: String
print_usage = "\n\nusage: gomoku world_type board_size target_size which_colour\
				\\n\t\t world_type: string argument to indicate if new game, load game\
				\\n default settings or settings in game\
        \[new || <name_of_load file> || def || ingame]\
				\\n\t\t board_size: allowed 3 <= size <= 19\
				\\n\t\t target_size: allowed 3 <= target <= 16\
				\\n\t\t which_colour: string arguments [Black || White]\
				\\n Please note that if you are using the default setting, do not pass\
				\ any other parameters, other than the string def.\n\n"

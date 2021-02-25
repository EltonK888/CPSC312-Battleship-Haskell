import Battleship
import System.IO
import Text.Read

-- sample start state
s1 = State (([], [[(1, 2), (1, 3), (1, 4)], [(2, 1), (2, 2)]]), ([], [[(4, 3), (4, 4), (4, 5)]])) (generateAvailableMoves)

play :: IO ()
play = playGame battleship s1 (-1, -1)

playGame :: Game -> State -> Coordinate -> IO ()
playGame game start_state player =
    do
        putStrLn("Enter Row: ")
        rowAsString <- getLine
        putStrLn("Enter Column: ")
        colAsString <- getLine
        let row = read rowAsString :: Int
        let col = read colAsString :: Int
        
        print(game start_state (row, col))
        let (ContinueGame s) = game start_state (row, col)
        playGame game s (row, col)
import Battleship
import System.IO
import Text.Read

-- sample start state
s1 = State (([], [[(1, 2), (1, 3), (1, 4)], [(2, 1), (2, 2)]]), ([], [[(4, 3), (4, 4), (4, 5)]])) (generateAvailableMoves)

play :: IO ()
play = playGame battleship (ContinueGame s1) (-1, -1)

playGame :: Game -> Result -> Coordinate -> IO ()
playGame game (EndOfGame 1.0 start_state) player = 
    do
        putStrLn("End of Game")
        return ()

playGame game (ContinueGame start_state) player =
    do
        putStrLn("Enter Row: ")
        rowAsString <- getLine
        putStrLn("Enter Column: ")
        colAsString <- getLine
        let row = read rowAsString :: Int
        let col = read colAsString :: Int
        
        print(game start_state (row, col))
        let s = game start_state (row, col)
        playGame game s (row, col)


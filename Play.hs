import Battleship
import System.IO
import Text.Read

-- sample start state
s1 = State (([], [[(1, 2), (1, 3), (1, 4)], [(2, 1), (2, 2)]]), ([], [[(4, 3), (4, 4), (4, 5)]])) (generateAvailableMoves)
s0 = State (([], [[]]), ([], [[]])) (generateAvailableMoves)
ships0 = [[(-1,-1)]]


play :: IO ()
play =  setup s0 3 ships0
        -- playGame battleship s0 (-1, -1)


setup :: State -> Int -> [Ship] -> IO ()
setup s n ships = 
    if (n < 2)
    then
        playGame battleship 
                -- init ships deletes the (-1,-1) used to initialize ships0 (the last element)
                (State (([], (init ships)), ([], [[]])) 
                    (generateAvailableMoves))
                (-1,-1)
    else
        do
            putStrLn("Enter size " ++ show n ++ " ship row: ")
            shipRowAsString <- getLine
            putStrLn("Enter size " ++ show n ++ " ship row: ")
            shipColAsString <- getLine
            putStrLn("facing which direction? (up, right, down, left): ")
            ostr <- getLine
            let row = read shipRowAsString :: Int
            let col = read shipColAsString :: Int
            let ship = generateShip (row,col) n ostr
            setup s (n-1) (ship:ships)


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
module Main where

import Battleship
import BattleshipGraphics
import BattleshipInputLogic
import Graphics.Gloss

import System.IO
import Text.Read

-- sample start state
--s1 = State (([], [[(1, 2), (1, 3), (1, 4)], [(2, 1), (2, 2)]]), ([], [[(4, 3), (4, 4), (4, 5)]])) (generateAvailableMoves)
s0 = State (([], [[]], [], generateAvailableMoves), ([], [[]], [], generateAvailableMoves))
ships0 = [[(-1,-1)]]


--play :: IO ()
--play =  setup s0 3 ships0
        -- playGame battleship s0 (-1, -1)


setup :: State -> Int -> [Ship] -> IO ()
setup s n ships =
    if (n < 2)
    then
        do
        cpuShips <- placeOppShips 5 []
        
        play window white 30 (ContinueGame (State (([], (init ships), [], generateAvailableMoves), ([], []:cpuShips, [], generateAvailableMoves)))) gameAsPicture event (const id)
        --playGame battleship
                -- init ships deletes the (-1,-1) used to initialize ships0 (the last element)
                --(ContinueGame (State (([], (init ships), []), ([], cpuShips, []))
                    --(generateAvailableMoves)))
                --(-1,-1)
    else
        do
            putStrLn("Enter size " ++ show n ++ " ship row[1-8]: ")
            shipRowAsString <- getLine
            putStrLn("Enter size " ++ show n ++ " ship col[1-8]: ")
            shipColAsString <- getLine
            putStrLn("facing which direction? (up, right, down, left): ")
            ostr <- getLine
            if elem ostr directions
              then do
              let row = read shipRowAsString :: Int
              let col = read shipColAsString :: Int
              let ship = generateShip (row,col) n ostr
              if validShip ship ships
                then do
                setup s (n-1) (ship:ships)
                else do
                  putStrLn("Invalid Ship Placement")
                  setup s n ships
              else do
                putStrLn("Could not parse direction")


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

window :: Display
window = InWindow "Battleship" (1200, 800) (100,100)

main :: IO ()
main = setup s0 5 ships0
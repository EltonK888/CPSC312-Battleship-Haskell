module Battleship where
import System.Random

maxRow :: Int
maxRow = 8

maxCol :: Int
maxCol = 8

directions = ["up", "right", "down", "left"]

data State = State InternalState
            deriving (Eq, Show)

--data Board = [[Int]]
data Result = EndOfGame Double State
            | ContinueGame State
            deriving (Eq, Show)
--type Board = [Coordinate]

-- Actions are a list of coordinates
type Actions = [Coordinate]

-- a Ship is just a list of coordinates
type Ship = [Coordinate]

-- a Board is also just a list of coordinates

type Player = State -> Coordinate

-- Coordinates is just Row and Column
type Coordinate = (Int, Int)

-- internal state which is just a list of your selected coordinates, ship locations, where you hit, and your available moves
--  and the computer's coordinates, ship locations, where it hit, and available moves
type InternalState = ((Actions, [Ship], Actions, Actions), (Actions, [Ship], Actions, Actions))

type Game = State -> Coordinate -> Result

--instance Eq State where =
        --(==) (State InternalState s1) (State InternalState s2) =

-- the Battleship Game
battleship :: Game
battleship (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves))) move
    | not (elem move myAvailMoves) = ContinueGame (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))
    -- | length availableMoves == 0 = EndOfGame 1.0 (State ((mymoves, myships, myhit), (oppMoves, oppShips, opphit)) availableMoves)
    | win move oppShips = EndOfGame 1.0 (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))
    | hit = ContinueGame (State ((oppMoves, oppNewShips, opphit, oppAvailMoves), (move:mymoves, myships, hitMove:myhit, [act | act <- myAvailMoves, act /= move])))
    | otherwise = ContinueGame (State ((oppMoves, oppNewShips, opphit, oppAvailMoves), (move:mymoves, myships, myhit, [act | act <- myAvailMoves, act /= move])))
    where (hit, oppNewShips, hitMove) = checkHit move oppShips

-- checks if a move ends up in a player winning
win :: Coordinate -> [Ship] -> Bool
win move [] = True
win move shipLocations
    | hit && (and (foldr (\x y -> if length x == 0 then True:y else False:y) [True] newShipLocations)) = True
    | otherwise = False
    where
        (hit, newShipLocations, _) = checkHit move shipLocations

-- checkHit takes in the move, the list of ships, and returns a tuple of if the move hit, updated list of ships, and where it hit
checkHit :: Coordinate -> [Ship] -> (Bool, [Ship], Coordinate)
checkHit move ships
    | or (foldr (\x y -> if elem move x then True:y else False:y) [] ships) = (True, [removeFromLst move x | x <- ships], move)
    | otherwise = (False, ships, move)

-- Removes the specified element from the list
removeFromLst :: (Eq a) => a -> [a] -> [a]
removeFromLst item lst = foldr (\x y -> if x == item then y else x:y) [] lst

-- generates a grid as a 2d array of Coordinates which is maxCol by maxRow
generateAvailableMoves :: Actions
generateAvailableMoves = [(row, col) | row <- [1..maxRow], col <- [1..maxCol]]

-- generates a ship's coordinates based on the shiphead's coordinates, and the orientation of the ship
generateShip :: Coordinate -> Int -> String -> Ship
generateShip _ 0 _ = []
generateShip (x,y) size orientation
    | orientation == "up" = (x, y - (size - 1)) : generateShip (x,y) (size-1) orientation
    | orientation == "right" = (x - (size - 1), y) : generateShip (x,y) (size-1) orientation
    | orientation == "down" = (x, y + (size - 1)) : generateShip (x,y) (size-1) orientation
    | otherwise = (x + (size - 1), y) : generateShip (x,y) (size-1) orientation


validShip :: Ship -> [Ship] -> Bool
validShip [] _ = True
validShip (h:t) ships
    | validCoord h && (not $ elem h $ shipsToCoord ships) = validShip t ships
    | otherwise = False

shipsToCoord :: [Ship] -> [Coordinate]
shipsToCoord [] = []
shipsToCoord (h:t) = h++shipsToCoord t

validCoord :: Coordinate -> Bool
validCoord (x,y) = x >= 1 && x <= maxRow && y >= 1 && y <= maxCol

placeOppShips :: Int -> [Ship] -> IO [Ship]
placeOppShips n ships =
  if n < 2 then
      do
          return ships
  else 
    do
        putStrLn("Player 2 Enter size " ++ show n ++ " ship row[1-8]: ")
        shipRowAsString <- getLine
        putStrLn("Player 2 Enter size " ++ show n ++ " ship col[1-8]: ")
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
                placeOppShips (n-1) (ship:ships)
            else do
                putStrLn("Invalid Ship Placement")
                placeOppShips n ships
            else do
                putStrLn("Could not parse direction")
                placeOppShips n ships
    --ships <- randomlyPlaceShip 5 ships
    --ships <- randomlyPlaceShip 4 ships
    --ships <- randomlyPlaceShip 3 ships
    --ships <- randomlyPlaceShip 2 ships
    --return ships

randomlyPlaceShip :: Int -> [Ship] -> IO [Ship]
randomlyPlaceShip n ships =
  do
    g <- newStdGen
    let coord = ( ((randomRs (1,maxRow) g) !! 0), ((randomRs (1,maxCol) g) !! 1) )
    let dir = directions !! (head (randomRs (0,3) g))
    let newShip = generateShip coord n dir
    if validShip newShip ships
      then return (newShip:ships)
      else  randomlyPlaceShip n ships

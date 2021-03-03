module Battleship where

maxRow = 10
maxCol = 10

data State = State InternalState Actions
            deriving (Eq, Show)


data Result = EndOfGame Double State
            | ContinueGame State
            deriving (Eq, Show)

-- Actions are a list of coordinates
type Actions = [Coordinate]

-- a Ship is just a list of coordinates
type Ship = [Coordinate]

type Player = State -> Coordinate

-- Coordinates is just Row and Column
type Coordinate = (Int, Int)

-- internal state which is just a list of your selected coordinates and ship locations, and the computer's coordinates and ship locations
type InternalState = ((Actions, [Ship]), (Actions, [Ship]))

type Game = State -> Coordinate -> Result

--instance Eq State where =
        --(==) (State InternalState s1) (State InternalState s2) =

-- the Battleship Game
battleship :: Game
battleship (State ((mymoves, myships), (oppMoves, oppShips)) availableMoves) move
    | not (elem move availableMoves) = ContinueGame (State ((mymoves, myships), (oppMoves, oppShips)) availableMoves)
    | length availableMoves == 0 = EndOfGame 1.0 (State ((mymoves, myships), (oppMoves, oppShips)) availableMoves)
    | win move oppShips = EndOfGame 1.0 (State ((mymoves, myships), (oppMoves, oppShips)) availableMoves)
    | otherwise = ContinueGame (State ((oppMoves, oppNewShips), (move:mymoves, myships)) [act | act <- availableMoves, act /= move])
    where (_, oppNewShips) = checkHit move oppShips

-- checks if a move ends up in a player winning
win :: Coordinate -> [Ship] -> Bool
win move [] = True
win move shipLocations
    | hit && (and (foldr (\x y -> if length x == 0 then True:y else False:y) [True] newShipLocations)) = True
    | otherwise = False
    where
        (hit, newShipLocations) = checkHit move shipLocations

-- checkHit takes in the move, the list of ships, and returns a tuple of if the move hit and updated list of ships
checkHit :: Coordinate -> [Ship] -> (Bool, [Ship])
checkHit move ships
    | or (foldr (\x y -> if elem move x then True:y else False:y) [] ships) = (True, [removeFromLst move x | x <- ships])
    | otherwise = (False, ships)

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
validCoord (x,y) = x >= 1 && x <= 10 && y >= 1 && y <= 10

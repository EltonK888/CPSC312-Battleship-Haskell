module Battleship where

maxRow :: Int
maxRow = 8

maxCol :: Int
maxCol = 8

data State = State InternalState Actions
            deriving (Eq, Show)


data Result = EndOfGame Double State
            | ContinueGame State
            deriving (Eq, Show)
type Board = [Coordinate]

-- Actions are a list of coordinates
type Actions = [Coordinate]

-- a Ship is just a list of coordinates
type Ship = [Coordinate] 

-- a Board is also just a list of coordinates

type Player = State -> Coordinate

-- Coordinates is just Row and Column
type Coordinate = (Int, Int)

-- internal state which is just a list of your coordinates and ship locations, and the computer's coordinates and ship locations
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


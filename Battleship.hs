module BattleShip where

import System.Random
import Text.Read   (readMaybe)
import System.IO

data State = State ((Board, [Ship]), (Board, [Ship]), [Coordinate])
            deriving (Eq, Show)
data Result = EndOfGame Double State
            | ContinueGame State
            deriving (Eq, Show)

type Coordinate = (Int, Int)
-- Ship is a list of coordinates
type Ship = [Coordinate]
-- Board is a 10x10 array where 0 means a area that has not been fired and 1 meaning an area not containing a ship that has been fired and 2 meaning a ship that has been sunk
type Board = [[Char]]
-- A player takes a State and returns a Coordinate
type Player = State -> Coordinate
type Game = State -> Coordinate -> Result
-- InternalState is your board and list of ships, and the opponent board and their ships
--type InternalState = ((Board, [Ship]), (Board, [Ship]))
boardSize = 10
shipSize = 5
directions = ["up","down","left","right"]
legalCoord = ["1","2","3","4","5","6","7","8","9","10"]
battleship_start = State ((startBoard,[[(1,1),(1,2)]]), (startBoard,[[(3,1),(3,2)]]), generateAvailableMoves)

-- Create starting board of size 10x10 with all 0's
startBoard::Board
startBoard = replicate boardSize (replicate boardSize '~')

-- Change n-th element in a list
replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n-1) xs ++ [x] ++ drop n xs

removeFromLst :: (Eq a) => a -> [a] -> [a]
removeFromLst item lst = foldr (\x y -> if x == item then y else x:y) [] lst

battleship:: Game
battleship (State ((myBoard, myShips), (oppBoard,oppShips), avail)) move
  | length oppNewShips == 1 && length (head oppNewShips) == 0 && hit = EndOfGame 1.0 battleship_start
  | otherwise = ContinueGame (State ((newBoard, removeEmptyShips oppNewShips), (myBoard, myShips), avail))
  where (hit, oppNewShips, newBoard) = checkHit move oppShips oppBoard

removeEmptyShips :: [Ship] -> [Ship]
removeEmptyShips [] = []
removeEmptyShips (x:xs) | null x    = removeEmptyShips xs
                            | otherwise = x : removeEmptyShips xs
  -- checkHit takes in the move, the list of ships and board, and returns a tuple of if the move hit and updated list of ships and board
checkHit :: Coordinate -> [Ship] -> Board -> (Bool, [Ship], Board)
checkHit move ships b
    | or (foldr (\x y -> if elem move x then True:y else False:y) [] ships) = (True, [removeFromLst move x | x <- ships], fillBoard move b 'X')
    | otherwise = (False, ships, fillBoard move b '\'')

-- fills board at a coordinate with given input value
fillBoard :: Coordinate -> Board -> Char -> Board
fillBoard (x,y) b  val = replace x b (replace y (b !! (x-1)) val)

placeShips :: Int -> [Ship] -> IO [Ship]
placeShips size ships =
  if size <= shipSize then
    do
      ship <- inputShip size ships
      allShips <- placeShips (size + 1) (ship : ships)
      return (ship : allShips)
  else return []



inputShip :: Int -> [Ship] -> IO Ship
inputShip len ships = do
    putStrLn("Enter size " ++ show len ++ " ship row[1-10]: ")
    shipRowAsString <- getLine
    if elem shipRowAsString legalCoord
      then do
        let row = read shipRowAsString:: Int
        putStrLn("Enter size " ++ show len ++ " ship col[1-10]: ")
        shipColAsString <- getLine
        if elem shipColAsString legalCoord
          then do
            let col = read shipColAsString :: Int
            putStrLn("facing which direction? [up, right, down, left]: ")
            ostr <- getLine
            if elem ostr directions
              then do
                let ship = generateShip (row,col) len ostr
                if validShip ship ships then do
                  return ship
                else do
                  putStrLn("Ship is out of bounds or on top of another ship!")
                  inputShip len ships
              else do
                putStrLn("Incorect direction please select one of: [up,right,down,left]")
                inputShip len ships
            else do
              putStrLn("Incorrect Column")
              inputShip len ships
        else do
          putStrLn("Incorrect Row")
          inputShip len ships
-- generates a ship's coordinates based on the shiphead's coordinates, and the orientation of the ship
generateShip :: Coordinate -> Int -> String -> Ship
generateShip _ 0 _ = []
generateShip (x,y) size orientation
  | orientation == "up" = (x - (size - 1), y) : generateShip (x,y) (size-1) orientation
  | orientation == "left" = (x , y - (size - 1)) : generateShip (x,y) (size-1) orientation
  | orientation == "down" = (x + (size - 1), y) : generateShip (x,y) (size-1) orientation
  | otherwise = (x , y + (size - 1)) : generateShip (x,y) (size-1) orientation


validShip :: Ship -> [Ship] -> Bool
validShip [] _ = True
validShip (h:t) ships
    | validCoord h && (not $ elem h $ shipsToCoord ships) = validShip t ships
    | otherwise = False

shipsToCoord :: [Ship] -> [Coordinate]
shipsToCoord [] = []
shipsToCoord (h:t) = h++shipsToCoord t

validCoord :: Coordinate -> Bool
validCoord (x,y) = x >= 1 && x <= boardSize && y >= 1 && y <= boardSize

--play :: Board -> Board -> [Ship] -> [Ship] -> IO()
--play b1 b2 s1 s2 = do

generateOpponentShips :: Int -> [Ship] -> IO [Ship]
generateOpponentShips size ships =
  if size <= shipSize then
    do
      ship <- generateRandomShip size ships
      shipsSoFar <- generateOpponentShips (size + 1) (ship:ships)
      return (ship:shipsSoFar)
    else return []

generateRandomShip :: Int -> [Ship] -> IO Ship
generateRandomShip n ships =
  do
    g <- newStdGen
    let coord = ( ((randomRs (1,boardSize) g) !! 0), ((randomRs (1,boardSize) g) !! 1) )
    let dir = directions !! (head (randomRs (0,3) g))
    let newShip = generateShip coord n dir
    if validShip newShip ships
      then return newShip
      else  generateRandomShip n ships


person_play :: Game -> Result -> Player -> IO ()
person_play game (ContinueGame state) opponent =
  do
    let State ((b1,s1),(b2,s2), avail) = state
    --putStrLn(show s2)
    putStrLn("================Enemy Board================")
    printBoard b2
    putStrLn("================Your Board================")
    printBoard b1
    putStrLn("Enter Row[1-10]: ")
    rowAsString <- getLine
    if elem rowAsString legalCoord
      then do
        let row = read rowAsString :: Int
        putStrLn("Enter Column[1-10]: ")
        colAsString <- getLine
        if elem colAsString legalCoord
          then do
            let col = read colAsString :: Int
            putStrLn("You are firing at Row: "++rowAsString++ " Column: "++colAsString)
            computer_play game (game state (row,col)) opponent
          else do
            putStrLn("Incorrect Column")
            person_play game (ContinueGame state) opponent
        else do
          putStrLn("Incorrect Row")
          person_play game (ContinueGame state) opponent

person_play game (EndOfGame val battleship_start) opponent =
  do
    putStrLn ("Computer Won! Too Bad")

computer_play :: Game -> Result -> Player -> IO()
computer_play game (EndOfGame val battleship_start) opponent =
  do
    putStrLn ("You Won!")
computer_play game (ContinueGame state) opponent =
  let
    opponent_move = opponent state
      in
        do
          let State ((b1,s1),(b2,s2), avail) = state
          putStrLn("The computer fired at "++show opponent_move)
          let newMoveSet = removeFromLst opponent_move avail
          let newState = State ((b1,s1),(b2,s2), newMoveSet)
          person_play game (game newState opponent_move) opponent
play:: IO()
play = do
  putStrLn("Place down your ships")
  playerShips <- placeShips 2 []
  oppShips <- generateOpponentShips 2 []
  let newState = State ((fillShips (shipsToCoord playerShips) startBoard,playerShips), (startBoard,oppShips), generateAvailableMoves)
  putStrLn("Select Difficulty: \n [0] Impossible  \n [1] Easy \n [2] Normal (Default)")
  diff<- getLine
  if diff == "0"
    then
      person_play battleship (ContinueGame newState) impossible_player
    else if diff == "1"
      then
        person_play battleship  (ContinueGame newState) seq_player
    else person_play battleship (ContinueGame newState) random_player

fillShips::[Coordinate] -> Board -> Board
fillShips coords b = foldr (\x y -> fillBoard x y 'S') b coords

impossible_player::Player
impossible_player (State ((cpuB,cpuS),(pB,pS),avail)) = (head (head pS))

seq_player::Player
seq_player (State ((cpuB,cpuS),(pB,pS),avail)) = head avail

random_player::Player
random_player (State ((cpuB,cpuS),(pB,pS),avail)) = pickRandomAvail avail (head $ head cpuS) (head $ head pS)

generateAvailableMoves :: [Coordinate]
generateAvailableMoves = [(row, col) | row <- [1..boardSize], col <- [1..boardSize]]

pickRandomAvail :: [Coordinate] -> Coordinate -> Coordinate -> Coordinate
pickRandomAvail coords (a,b) (c,d) = let gen = mkStdGen (length coords + a + b*10 + c*100 + d*1000)
 in coords !! (head (randomRs (0,(length coords)-1) gen))

printBoard::Board -> IO()
printBoard b = do
  putStrLn("0   1   2   3   4   5   6   7   8   9   10 \n")
  putStrLn("1   "++(printRow $ b!!0) ++ "\n")
  putStrLn("2   "++(printRow $ b!!1) ++ "\n")
  putStrLn("3   "++(printRow $ b!!2) ++ "\n")
  putStrLn("4   "++(printRow $ b!!3) ++ "\n")
  putStrLn("5   "++(printRow $ b!!4) ++ "\n")
  putStrLn("6   "++(printRow $ b!!5) ++ "\n")
  putStrLn("7   "++(printRow $ b!!6) ++ "\n")
  putStrLn("8   "++(printRow $ b!!7) ++ "\n")
  putStrLn("9   "++(printRow $ b!!8) ++ "\n")
  putStrLn("10  "++(printRow $ b!!9) ++ "\n")
--  putStrLn("1 \t "++ printRow (b !! 1) 0)

printRow :: Foldable t => t Char -> [Char]
printRow b = foldr (\ x y -> (x:"   ")++y) [] b


stringMultiply :: String -> Int -> String
stringMultiply string n = concat $ replicate n string

import System.Random

data State = State ((Board, [Ship]), (Board, [Ship]))
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
shipSize = 3
directions = ["up","down","left","right"]

battleship_start = State ((startBoard,[[(1,1),(1,2)]]), (startBoard,[[(3,1),(3,2)]]))

-- Create starting board of size 10x10 with all 0's
startBoard::Board
startBoard = replicate 10 (replicate 10 'O')

-- Select the n-th element in a list
select :: Int -> [a] -> a
select n xs = head (drop (n-1) (take n xs))

-- Change n-th element in a list
replace :: Int -> [a] -> a -> [a]
replace n xs x = take (n-1) xs ++ [x] ++ drop n xs

removeFromLst :: (Eq a) => a -> [a] -> [a]
removeFromLst item lst = foldr (\x y -> if x == item then y else x:y) [] lst

battleship:: Game
battleship (State ((myBoard, myShips), (oppBoard,oppShips))) move
  | length (head oppShips) == 1 && hit = EndOfGame 1.0 battleship_start
  | otherwise = ContinueGame (State ((newBoard, removeEmptyShips oppNewShips), (myBoard, myShips)))
  where (hit, oppNewShips, newBoard) = checkHit move oppShips oppBoard

removeEmptyShips :: [Ship] -> [Ship]
removeEmptyShips [] = []
removeEmptyShips (x:xs) | null x    = removeEmptyShips xs
                            | otherwise = x : removeEmptyShips xs
  -- checkHit takes in the move, the list of ships and board, and returns a tuple of if the move hit and updated list of ships and board
checkHit :: Coordinate -> [Ship] -> Board -> (Bool, [Ship], Board)
checkHit move ships b
    | or (foldr (\x y -> if elem move x then True:y else False:y) [] ships) = (True, [removeFromLst move x | x <- ships], fillBoard move b 'X')
    | otherwise = (False, ships, fillBoard move b 'I')

-- fills board at a coordinate with given input value
fillBoard :: Coordinate -> Board -> Char -> Board
fillBoard (x,y) b  val = replace x b (replace y (select x b) val)

placeShips :: Int -> [Ship] -> IO [Ship]
placeShips size ships =
  if size <= shipSize then
    do
      ship <- placeShip size ships
      allShips <- placeShips (size + 1) (ship : ships)
      return (ship : allShips)
  else return []



placeShip :: Int -> [Ship] -> IO Ship
placeShip len ships = do
    putStrLn("Enter size " ++ show len ++ " ship row[1-10]: ")
    shipRowAsString <- getLine
    putStrLn("Enter size " ++ show len ++ " ship col[1-10]: ")
    shipColAsString <- getLine
    putStrLn("facing which direction? (up, right, down, left): ")
    ostr <- getLine
    let row = read shipRowAsString :: Int
    let col = read shipColAsString :: Int
    let ship = generateShip (row,col) len ostr
    if validShip ship ships then
      return ship
    else
      placeShip len ships

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
validCoord (x,y) = x >= 1 && x <= 10 && y >= 1 && y <= 10

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
    let coord = ( ((randomRs (1,10) g) !! 0), ((randomRs (1,10) g) !! 1) )
    let dir = directions !! (head (randomRs (0,3) g))
    let newShip = generateShip coord n dir
    if validShip newShip ships
      then return newShip
      else  generateRandomShip n ships


person_play :: Game -> Result -> IO ()
person_play game (ContinueGame state) =
  do
    let State ((b1,s1),(b2,s2)) = state
    putStrLn (show b1)
    putStrLn (show s1)
    putStrLn (show b2)
    putStrLn (show s2)
    putStrLn("Enter Row: ")
    rowAsString <- getLine
    putStrLn("Enter Column: ")
    colAsString <- getLine
    let row = read rowAsString :: Int
    let col = read colAsString :: Int
    person_play game (game state (row,col))

person_play game (EndOfGame val battleship_start) =
  do
    putStrLn ("Game Over")


play:: IO()
play = do
  putStrLn("Place down your ships")
  playerShips <- placeShips 2 []
  oppShips <- generateOpponentShips 2 []
  let newState = State ((fillShips (shipsToCoord playerShips) startBoard,playerShips), (startBoard,oppShips))
  person_play battleship (ContinueGame newState)

fillShips::[Coordinate] -> Board -> Board
fillShips coords b = foldr (\x y -> fillBoard x y 'S') b coords

-- Scrap from Play.hs ---------------------------------------

-- setup :: State -> IO ()
-- setup s =
--     do 
--         putStrLn("Enter size 2 ship row: ")
--         shipRowAsString <- getLine
--         putStrLn("Enter size 2 ship row: ")
--         shipColAsString <- getLine
--         -- putStrLn("facing which direction? (up, right, down, left): ")
--         -- ostr <- getLine

--         let rship2 = read shipRowAsString :: Int
--         let cship2 = read shipColAsString :: Int
--         playGame battleship 
--                 (State (([], [[(rship2, cship2), (rship2+1, cship2)]]), ([], [[]])) 
--                     (generateAvailableMoves))
--                 (-1,-1)


-- Scrap from Battleship.hs ---------------------------------------

-- data Orientation = Up | Right | Down | Left | Unknown
--             deriving (Eq, Show)
module BattleshipGraphics where

import Graphics.Gloss
import Battleship

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

-- creates a grid of maxRow by maxCol size
grid :: [Point]
grid = [(x, y) | x <- [1.0..(fromIntegral maxRow)], y <- [1.0..(fromIntegral maxCol)]]

-- creates the picture of the grid
gridToPicture :: Picture
gridToPicture = pictures (map (\(x, y) -> translate (50.0*x) (50.0*y) (rectangleWire 50.0 50.0)) grid)

-- maps opponent's hit squares to a picture
cpuHitSquares :: [Point] -> Picture
cpuHitSquares cpuHitCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color orange (rectangleSolid 50.0 50.0)))) cpuHitCoords)

-- maps the squares you've hit to a picture (red box)
hitSquares :: [Point] -> Picture
hitSquares hitCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color red (rectangleSolid 50.0 50.0)))) hitCoords)

-- maps the sqaures you've missed to a picture (grey box)
missedSquares :: [Point] -> Picture
missedSquares missedCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color (greyN 0.5) (rectangleSolid 50.0 50.0)))) missedCoords)

-- maps the available moves to a picture (blue box)
availableSquares :: [Point] -> Picture
availableSquares availableCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color blue (rectangleSolid 50.0 50.0)))) availableCoords)

-- maps your ships to a picture (green box)
myShipsToPicture :: [[Point]] -> Picture
myShipsToPicture myships = pictures (map (\ship -> pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color green (rectangleSolid 50.0 50.0)))) ship)) myships)

-- converts a list of ships into points so that Gloss can work with the coordinates
shipsToPoints :: [Ship] -> [[Point]]
shipsToPoints ships = [[(fromIntegral x, fromIntegral y) | (x,y) <- ship] | ship <-ships ]

-- fixes the Y coordinate bug
fixYCoord :: Num b => Int -> b
fixYCoord y = fromIntegral (maxCol - (y-1))

-- creates a picture representation of the game
gameAsPicture :: Result -> Picture
gameAsPicture (EndOfGame _ (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))) 
    | (length oppShips) == 6 = translate (-400) (0) (text "Player 1 wins!")
    | otherwise = translate (-400) (0) (text "Player 2 wins!")
gameAsPicture (ContinueGame (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))) = translate (-50.0 * 4) (-50.0 * 4) (pictures [
    availableSquares [(fromIntegral x, fixYCoord (fromIntegral y)) | (x,y) <- myAvailMoves],
    missedSquares [(fromIntegral (fst x), fixYCoord (fromIntegral (snd x))) | x <- mymoves, not (elem x myAvailMoves)],
    -- cpuHitSquares [(fromIntegral x, fromIntegral y) | (x,y) <- opphit],
    --myShipsToPicture (shipsToPoints myships),
    hitSquares [(fromIntegral x, fixYCoord(fromIntegral y)) | (x,y) <- myhit],
    gridToPicture
    ])
module BattleshipGraphics where

import Graphics.Gloss
import Battleship

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

grid :: [Point]
grid = [(x, y) | x <- [1.0..(fromIntegral maxRow)], y <- [1.0..(fromIntegral maxCol)]]

gridToPicture :: Picture
gridToPicture = pictures (map (\(x, y) -> translate (50.0*x) (50.0*y) (rectangleWire 50.0 50.0)) grid)

cpuHitSquares :: [Point] -> Picture
cpuHitSquares cpuHitCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color orange (rectangleSolid 50.0 50.0)))) cpuHitCoords)

hitSquares :: [Point] -> Picture
hitSquares hitCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color red (rectangleSolid 50.0 50.0)))) hitCoords)

missedSquares :: [Point] -> Picture
missedSquares missedCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color (greyN 0.5) (rectangleSolid 50.0 50.0)))) missedCoords)

availableSquares :: [Point] -> Picture
availableSquares availableCoords = pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color blue (rectangleSolid 50.0 50.0)))) availableCoords)

myShipsToPicture :: [[Point]] -> Picture
myShipsToPicture myships = pictures (map (\ship -> pictures (map (\(x, y) -> (translate (50.0*x) (50.0*y) (color green (rectangleSolid 50.0 50.0)))) ship)) myships)

shipsToPoints :: [Ship] -> [[Point]]
shipsToPoints ships = [[(fromIntegral x, fromIntegral y) | (x,y) <- ship] | ship <-ships ]

gameAsPicture :: Result -> Picture
gameAsPicture (EndOfGame _ s) = translate (-200) (-200) (Text "End of Game")
gameAsPicture (ContinueGame (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))) = translate (-50.0 * 4) (-50.0 * 4) (pictures [
    availableSquares [(fromIntegral x, fromIntegral y) | (x,y) <- myAvailMoves],
    missedSquares [(fromIntegral (fst x), fromIntegral (snd x)) | x <- mymoves, not (elem x myAvailMoves)],
    -- cpuHitSquares [(fromIntegral x, fromIntegral y) | (x,y) <- opphit],
    myShipsToPicture (shipsToPoints myships),
    hitSquares [(fromIntegral x, fromIntegral y) | (x,y) <- myhit],
    gridToPicture
    ])
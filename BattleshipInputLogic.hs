module BattleshipInputLogic where

import Battleship
import BattleshipGraphics
import Graphics.Gloss.Interface.Pure.Game


event :: Event -> Result -> Result
event (EventKey (MouseButton LeftButton) Up _ (mx, my)) (ContinueGame (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves))))
    | not (elem mousePos myAvailMoves) = ContinueGame (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))
    -- | hitOwn = ContinueGame (State ((mymoves, myships, myhit), (oppMoves, oppShips, opphit)) availableMoves)
    | length myAvailMoves == 0 = EndOfGame 1.0 (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))
    | win mousePos oppShips = EndOfGame 1.0 (State ((mymoves, myships, myhit, myAvailMoves), (oppMoves, oppShips, opphit, oppAvailMoves)))
    | hit = ContinueGame (State ((oppMoves, oppNewShips, opphit, oppAvailMoves), (mousePos:mymoves, myships, mousePos:myhit, [act | act <- myAvailMoves, act /= mousePos])))
    | otherwise = ContinueGame (State ((oppMoves, oppNewShips, opphit, oppAvailMoves), (mousePos:mymoves, myships, myhit, [act | act <- myAvailMoves, act /= mousePos])))
    where
        mousePos = (mapMousePos mx, mapMousePos my)
        (hit, oppNewShips, hitMove) = checkHit mousePos oppShips
        -- (hitOwn, _, _) = checkHit mousePos myships
event _ world = world

mapMousePos x = fromIntegral (round ((x + (50.0*((fromIntegral maxCol)/2)))/50))

--transformGame _ game =
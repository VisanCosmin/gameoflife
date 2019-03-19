module Main where


import Game

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Matrix
-- Blue 207,222,243
-- Yellow #ffd454 
-- Red #d02e2d  
-- Blue #1a2f7d

data Game = Game { world :: Matrix Cell
                 , x :: Float 
                 , y :: Float
                 , zoom :: Float 
                 , deltaTime :: Float 
                 , fps :: Float
                 , run :: Bool 
                 , generation :: Int}


main :: IO ()
main = do
    play (InWindow "GameEvent" (1000, 800) (300, 100))
         (makeColorI 255 212 84 255)
         30 -- FPS
         initGame
         drawWorld
         eventUpdate
         timeUpdate

    where drawWorld game = pictures [ translate (-510) (-410) $ drawMatrix (world game) 
                                    , translate (-480) 370 $ scale 0.2 0.2 $ text $ (show $ generation game) ++ "  " ++ isRunning (run game) ]
                           where isRunning True = "Running"
                                 isRunning False = "Paused" 
          eventUpdate (EventKey (MouseButton LeftButton) Down _ co)  game = game { world = setCell Alive (coordsToPos co) (world game) }
          eventUpdate (EventKey (MouseButton RightButton) Down _ co) game = game { world = setCell Dead (coordsToPos co) (world game) }
          eventUpdate (EventKey (SpecialKey KeySpace) Down _ _)      game = game { run = not (run game)}
          eventUpdate _ game = game
          timeUpdate dt game =  case (run game) of True -> if (dt + (deltaTime game) > 1/(fps game)) 
                                                                                then game { world = stepWorld (world game) , deltaTime = 0 , generation = (generation game) +1}
                                                                                else game { deltaTime = (deltaTime game) + dt}
                                                   False -> game 

drawCell :: Int -> Int -> Picture
drawCell x y = translate ((fromIntegral x)* 20) ((fromIntegral y)* 20) cellPicture

drawMatrix :: Matrix Cell -> Picture
drawMatrix world = pictures [ drawCell x y
                            | x <- [1,2..size] , y <- [1,2..size]
                            , ((world ! (x,y)) == Alive)]
                   where size = nrows world

cellPicture :: Picture
cellPicture = rectangleSolid 20 20

coordsToPos :: (Float,Float) -> (Int,Int)
coordsToPos (x,y) = (ceiling ((x+500)/20), ceiling ((y+400)/20))

initGame :: Game 
initGame = Game { world = createWorld 55
                  -- foldl (.) id (fmap (\x -> setCell Alive x) [(20,20),(19,21) , (21,21) , (19,22) , (21,22) , (20,23)]) $ 
                , x = 0
                , y = 0
                , zoom = 1
                , deltaTime = 0
                , fps = 3 
                , run = False 
                , generation = 0}
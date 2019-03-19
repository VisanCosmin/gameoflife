module Game where

import Data.Matrix

data Cell = Alive | Dead deriving Eq

cellToInt :: Cell -> Int
cellToInt Alive = 1
cellToInt Dead  = 0

intToCell :: Cell -> Int -> Cell 
intToCell Alive x | x == 2 || x == 3 = Alive
intToCell Dead 3                     = Alive
intToCell _ _                        = Dead 

instance Show Cell where
    show Alive = "@"
    show Dead =  "-" 

createWorld :: Int -> Matrix Cell
createWorld size = matrix size size (\_ -> Dead)

setCell :: Cell -> (Int , Int ) -> Matrix Cell -> Matrix Cell 
setCell = setElem 


countCells :: Matrix Cell -> (Int ,Int) -> Int
countCells world (x,y) =  sum $ fmap (cellToInt . (world !)) 
                                     [(x+xs , y+ys) | xs <- [-1,0,1] 
                                                    , ys <- [-1,0,1] 
                                                    , ((xs /= 0) || (ys /= 0))
                                                    , x + xs >= 1
                                                    , y + ys >= 1 ] 


stepWorld :: Matrix Cell -> Matrix Cell 
stepWorld world = foldl (cellToCell world)
                        newWorld 
                        [(x,y) | x <-[1,2..size-1] , y <- [1,2..size-1]]

                  where newWorld = createWorld size
                        size = nrows world
                        cellToCell :: Matrix Cell -> Matrix Cell -> (Int,Int) -> Matrix Cell
                        cellToCell oldWorld world xy = setCell cell xy world
                                                       where cell = intToCell (oldWorld ! xy) $ countCells oldWorld xy


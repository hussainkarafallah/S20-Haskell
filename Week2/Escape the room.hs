{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import CodeWorld.Image


{-|
we are applying an interaction to the world consisting of (Map,Coord)
Map is the actual map presented by 2d array of tiles and Coord is tuple of 2 ints denoting position
each tile could be (door,key,wall,floor,exit)
exit is green
floor is blue
wall is black
key is a small circle in a floor tile
walls when locked have solid color (something you may decide)
walls when opened have a floor color framed with the original door color (for better distinction)
you should reach the exit
here i have a 10 by 10 map
to draw a new level just change the array at bottom of the code
-}

data Tile = Floor | Wall | Key Color  | Door Color Bool | Exit
data Dir = Up | Down | ToLeft | ToRight

success = image  "ending" "https://pics.me.me/thumb_very-nice-great-success-great-succes-borat-very-nice-50474139.png" 12 12
pacman = image "pac" "https://i7.pngguru.com/preview/238/58/398/5bbc055f37ea2.jpg" 0.7 0.7

type Map = [[Tile]]
type RowMap = [Tile] 
type Coord = (Int,Int)
type World = (Map,Coord)


checkExit :: Tile -> Bool
checkExit Exit = True
checkExit _ = False


-- a function to render a tile
drawTile :: Tile -> Picture
drawTile Floor = colored blue (solidRectangle 0.9 0.9)
drawTile Wall = colored black (solidRectangle 0.9 0.9)
drawTile (Key col) = (colored col (solidCircle 0.28)) <> (colored blue (solidRectangle 0.9 0.9))
drawTile (Door col False) = colored col (solidRectangle 0.9 0.9)
drawTile (Door col True) = colored blue (solidRectangle 0.7 0.7) <> colored col (solidRectangle 0.9 0.9)

drawTile Exit = colored green (solidRectangle 0.9 0.9)


drawTileAt :: Tile -> Integer -> Integer -> Picture
drawTileAt tile x y = translated (fromIntegral x)  (fromIntegral y) (drawTile tile)


-- a function to check if a certain tile can be walked on
isOpen :: Tile -> Bool
isOpen Wall = False
isOpen Floor = True
isOpen (Key _) = True
isOpen (Door _ state) = state
isOpen Exit = True



-- a function to draw one row of the map which is a simple list
drawRowMap :: RowMap -> Picture
drawRowMap [] = blank
drawRowMap (x:xs) = drawTile x <> translated 1 0 (drawRowMap xs)


-- a function to draw the entire map it draws row by row stacked
drawMap :: Map -> Picture
drawMap [] = blank
drawMap (x:xs) = drawRowMap x <> translated 0 (-1) (drawMap xs)


-- Now here's a function that takes a map as parameter and a color and flips the state of all doors having this color
toggleMap :: Map -> Color -> Map
toggleMap [] c = []
toggleMap (x:xs) c = toggleRow x c : toggleMap xs c

-- by the same recursive logic for drawing we have a separate function for toggling a single row
toggleRow :: RowMap -> Color -> RowMap
toggleRow [] c = []
toggleRow ( (Door color state) : xs) target 
  | color == target = ( Door color (not state) : toggleRow xs target )
  | otherwise = ( Door color state : toggleRow xs target )
toggleRow ( y : xs ) target = ( y : toggleRow xs target ) 


-- a function that returns the tile at certain position
tileAt :: Map -> Int -> Int -> Tile
tileAt tilemap x y = ((tilemap !! y) !! x)


-- a function to check if the pacman can move to a new position
-- it checks the values of coordinates if valid and the tile type (walkable or not)
moveableTo :: Map -> Coord -> Bool
moveableTo tileMap (x,y)
   | x < 0 || y < 0 || y >= length tileMap || x >= length (tileMap !! 0) = False
   | otherwise = isOpen (tileAt tileMap x y)



-- a function that's applied to a given position and makes the decision of movement if possible

tryMove :: Dir -> World -> World

tryMove _ (tileMap,(x,y))
  | isGameEnded (tileMap,(x,y)) = (tileMap,(x,y))

tryMove Up (tileMap,(x,y)) 
  | (moveableTo tileMap (x,y-1)) == True = moveTo (x,y-1) (tileMap,(x,y)) 
  | otherwise = (tileMap,(x,y)) 

tryMove Main.ToLeft (tileMap,(x,y)) 
  | (moveableTo tileMap (x-1,y)) == True = moveTo (x-1,y) (tileMap,(x,y)) 
  | otherwise = (tileMap,(x,y))
tryMove Main.ToRight (tileMap,(x,y)) 
  | (moveableTo tileMap (x+1,y)) == True = moveTo (x+1,y) (tileMap,(x,y)) 
  | otherwise = (tileMap,(x,y))

tryMove Down  (tileMap,(x,y))
    | (moveableTo tileMap (x,y+1) == True) = moveTo (x,y+1) (tileMap,(x,y)) 
    | otherwise = (tileMap,(x,y))
    
--when try move clears out necessary conditions it calls this function
-- this function moves the pacman to the new position and applies the tile effect (i.e key effect)
-- this way of separation makes the code more extendable if we want to add new types of tiles

moveTo :: Coord -> World -> World
moveTo (nx,ny) (tileMap,(x,y)) =  
  (applyTileEffect tileMap (tileAt tileMap nx ny),(nx,ny))

--this function applies the effect of visiting some tile to the map (we have only toggling doors)
applyTileEffect :: Map -> Tile -> Map
applyTileEffect tileMap (Key col) = toggleMap tileMap col
applyTileEffect tileMap _ = tileMap


--a function to check if the game ended
isGameEnded :: World -> Bool
isGameEnded (tileMap,(x,y)) = checkExit (tileAt tileMap x y)

--a function that renders the world after every event
visualizeWorld :: World -> Picture
visualizeWorld (tileMap,(x,y)) 
  | isGameEnded (tileMap,(x,y)) == True = success
  | otherwise = translated  (fromIntegral x - 0.5)  (fromIntegral (-y) + 0.5) pacman <> drawMap tileMap



-- the detector of key presses
movementHandler :: Event -> World -> World
movementHandler event
  | event == (KeyPress "Down") = tryMove Down
  | event == (KeyPress "Up") = tryMove Up
  | event == (KeyPress "Left") = tryMove ToLeft
  | event == (KeyPress "Right") = tryMove ToRight
  | otherwise = id




gg :: Map

gg = [ [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall],
  [Wall,Floor,Wall,Floor,Floor,Floor,Floor,Door red False,Exit,Wall],
  [Wall,Floor,Floor,Wall,Floor,Floor,Floor,Door red False, Door red False,Wall],
  [Wall,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Floor,Wall],
  [Wall,Wall,Floor,Wall,Floor,Wall,Wall,Floor,Key pink,Wall],
  [Wall,Floor,Floor,Floor,Floor,Wall,Floor,Wall,Wall,Wall],
  [Wall,Floor,Wall,Floor,Wall,Floor,Floor,Floor,Floor,Wall],
  [Wall,Door pink False,Door pink False,Door pink False,Door pink False,Door pink False,Door pink False,Door pink False,Door pink False,Wall],
  [Wall,Floor,Key red,Floor,Floor,Floor,Floor,Floor,Floor,Wall],
  [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]]



main :: IO()

main = activityOf initial movementHandler visualizeWorld
  where
    initial = (gg,(2,2))
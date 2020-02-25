{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import Data.List
import Data.Maybe


-- | A mark for tic-tac-toe.
data Mark = X | O
  deriving (Eq, Show)
-- | A cell is either empty or has a mark.
type Cell = Maybe Mark
-- | A board is a 2D grid of cells.
type Board = [[Cell]]

--defining cell with its coordinates in the board
type IndexedCell = (Cell , (Int , Int))

-- now each cell is a tuple with indices
type IndexedBoard = [[IndexedCell]]


-- | Draw a single mark (X or O).
drawMark :: Mark -> Picture
drawMark X = scaled 0.4 0.4 (rotated (pi/4) (solidRectangle 0.5 2 <> solidRectangle 2 0.5))
drawMark O = thickCircle 0.2 0.3

-- | Draw one board cell at given coordinates.
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j cell = translated x y (rectangle 1 1 <> cellPicture)
  where
    x = fromIntegral i
    y = fromIntegral j
    cellPicture =
      case cell of
        Nothing -> blank
        Just m -> drawMark m
        

-- a function to render indexed cell
drawIndexed :: IndexedCell -> Picture
drawIndexed (cell , (row,col)) = drawCellAt col row cell


-- a function that gives dimensions of 2d matrix
getDimensions :: [[a]] -> (Int , Int)
getDimensions [] = (0,0)
getDimensions x = (length x,length (x !! 0))

-- a function that generates the cartesian product of 2 lists
cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- a function to split a 1d array into 2d array like chunks
-- 2 [1,2,3,4] -> [[1,2],[3,4]]
-- not sure if not using recursion requirement applies to every function but couldn't do this one without

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list
    
-- a function that takes a board as argument and returns [(cell,row,column)]
-- meaning it assigns indexes to cells
indexBoard ::  Board -> IndexedBoard
indexBoard board = indexedBoard
  where
    -- first let's flatten the board
    flattened = concat board 
    -- we can just zip it with cartesian product of [0..rows-1] * [0..cols-1]
    zipped = zip flattened ( cartProd [0..(rows-1)] [0..(cols-1)] )
    -- divide the flattened board into chunks and stack them as rows
    indexedBoard = splitEvery cols zipped
    rows = fst(getDimensions board)
    cols = snd(getDimensions board)
    
-- NOTE :: i know it's not effecient this way to flatten and enumerate and then split
-- but I wanted to make the code simpler
    
   
-- a function that renders the whole board
-- it simply generates the picture of every cell and joins them with a fold
drawBoard :: Board -> Picture
drawBoard board = drawWinnerStreak board <> (foldr (<>) blank (map drawIndexed flatIndexed)) 
  where
    flatIndexed = concat (indexBoard board)

-- a function that draws the streak
drawWinnerStreak :: Board -> Picture
drawWinnerStreak board 
  | isNothing winningStreak = blank
  | otherwise = (foldr (<>) blank (map ((colored red) . drawIndexed) (fromJust winningStreak)))  
  where
    winningStreak =  (getWinnerStreak board)
    
-- a function that updates some element given the idnex in 2d array
-- in fact i won't need specific function for our case so just wrote this one
updateAt :: (Int, Int) -> (a -> a) -> [[a]] -> [[a]]
updateAt (r,c) f matrix 
  -- check index
  | r < 0 || r >= rows || c < 0 || c >= cols = matrix
  |  otherwise = result
  where
    -- let's flatten the matrix for ease
    flattened = concat matrix
    -- let's split before updated item
    prefix = take (r * cols + c) flattened
    -- split after it
    suffix = drop(r * cols + c + 1) flattened
    -- find the value of it after applying a function (or update)
    updated = f (flattened !! (r * cols + c) ) 
    -- find the flattened updated matrix
    flatUpdated = prefix ++ (updated : suffix)
    -- rearrange it into 2d
    result = splitEvery cols flatUpdated
      
    rows = fst(getDimensions matrix)
    cols = snd(getDimensions matrix)
     
--  a function that tries to write on a cell (in case it has some mark in it it's not overwritten and nothing changes)
tryWrite :: Cell -> Cell -> Cell
tryWrite Nothing (Just c) = Just c
tryWrite x _  = x

-- a function that checks which player is going next by counting xs and os
nextToken :: Board -> Cell
nextToken board  
  | xs <= os = Just X
  | otherwise = Just O
  where
    xs = length (filter (\x -> x == Just X) (concat board))
    os = length (filter (\x -> x == Just O) (concat board))


--simple game handler
handleGame :: Event -> Board -> Board     
handleGame (PointerPress mouse) board
  | not (isNothing (getWinnerStreak board)) = board
  | otherwise = updateAt (pointToCoords mouse) (\x -> tryWrite x (nextToken board)) board
  
handleGame _ x = x

-- | Convert mouse position into board coordinates.

pointToCoords :: Point -> (Int, Int)
pointToCoords (x, y) = (round y, round x)    

-- | Sample 5x4 board.
sampleBoard :: Board
sampleBoard =
  [ [ x, o, n, o, n ]
  , [ n, o, n, x, o ]
  , [ x, n, x, n, n ]
  , [ n, o, n, x, x ] ]
  where
    (o, x, n) = (Just O, Just X, Nothing)

-- | Initialise an empty NxM board.
initBoard :: (Int, Int) -> Board
initBoard (n, m) = replicate m (replicate n Nothing)

-- a comparator for indexed cells (we just care about the mark inside)
areSameICells :: IndexedCell -> IndexedCell -> Bool
areSameICells (Nothing,_) _ = False
areSameICells _ (Nothing,_) = False
areSameICells (x,_) (y,_) = (x == y)

-- a function to check if indexed cell (first element of tuple is nothing)
isEmpty :: IndexedCell -> Bool
isEmpty (Nothing,_) = True
isEmpty _ = False

-- a function that returns a lsit containing indexed cells of the current streak on the map if it exists
getWinnerStreak :: Board -> Maybe [IndexedCell]
getWinnerStreak board = getTheStreak (filter isLongStreak (concatMap (streaks areSameICells isEmpty) allLines))
  where
    allLines = rows ++ columns ++ diagonals
    rows = indexBoard board
    columns = transpose rows
    diagonals = leftDiagonals ++ rightDiagonals
    leftDiagonals = leftDiagonalsOf rows
    rightDiagonals = leftDiagonalsOf (reverse rows)
    leftDiagonalsOf b = leftTopDiagonalsOf b ++ leftBottomDiagonalsOf b
    leftTopDiagonalsOf = transpose . zipWith drop [0..]
    leftBottomDiagonalsOf = leftTopDiagonalsOf . transpose


-- Get all consequent streaks ignoring 'Nothing'. it returns a list with indexed cells corresponding to the streak
-- i added another parameter which is comparison function to make it work for indexed cells
-- it also needs a function so we can ignore indexed cells that has nothing inside
streaks ::  (a->a->Bool) -> (a->Bool) -> [a] -> [[a]]
streaks _ _ [] = []
streaks eq isEmp (x : xs) 
  | isEmp x = streaks eq isEmp xs
  | otherwise = (x : ys) : streaks eq isEmp zs
  where
    (ys, zs) = span (eq x) xs


  
-- | Determine is a streak is long enough to be a winning streak.
isLongStreak :: [a] -> Bool
isLongStreak i = (length i) >= 3

-- | Get the list of cells corresponding to the streak
getTheStreak :: [[a]] -> Maybe [a]
getTheStreak [] = Nothing
getTheStreak (x:_)= Just x

-- | Run a tic-tac-toe game with a sample starting board.
main :: IO()
main = activityOf sampleBoard handleGame drawBoard
    
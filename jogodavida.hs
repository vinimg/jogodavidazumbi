type Cell = Int
type Grid = [[Cell]]

countNeighbors :: Grid -> Int -> Int -> (Int, Int)
countNeighbors grid x y = (countLiveNeighbors grid x y, countZombieNeighbors grid x y)
  where
    countLiveNeighbors grid x y = length [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y, isValidIndex grid i j, grid !! i !! j == 1]
    countZombieNeighbors grid x y = length [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y, isValidIndex grid i j, grid !! i !! j == 2]

isValidIndex :: Grid -> Int -> Int -> Bool
isValidIndex grid x y = x >= 0 && x < length grid && y >= 0 && y < length (head grid)

nextGeneration :: Grid -> Grid
nextGeneration grid = [[nextState grid i j | j <- [0..length (head grid) - 1]] | i <- [0..length grid - 1]]
  where
    nextState grid x y
      | grid !! x !! y == 0 = if countLiveNeighbors x y == 3 then 1 else 0
      | grid !! x !! y == 1 =
        if countZombieNeighbors x y >= 1 || countLiveNeighbors x y < 2 || countLiveNeighbors x y > 3
          then 0
          else 1
      | grid !! x !! y == 2 = if countLiveNeighbors x y == 0 then 0 else 2
      where
        countLiveNeighbors x y = length [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y, isValidIndex grid i j, grid !! i !! j == 1]
        countZombieNeighbors x y = length [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y, isValidIndex grid i j, grid !! i !! j == 2]



printGrid :: Grid -> IO ()
printGrid grid = mapM_ printRow grid
  where
    printRow row = putStrLn (concatMap show row)

simulateGame :: Grid -> Int -> IO ()
simulateGame grid iterations = simulate grid iterations 0
  where
    simulate grid remainingSteps currentStep
      | remainingSteps == 0 || not hasChanges = do
          putStrLn $ "Sistema estabilizado após " ++ show currentStep ++ " iterações."
          printGrid grid
      | otherwise = do
          putStrLn $ "Iteração " ++ show (currentStep + 1) ++ ":"
          printGrid grid
          simulate nextGrid (remainingSteps - 1) (currentStep + 1)
      where
        (nextGrid, hasChanges) = computeNextGeneration grid

computeNextGeneration :: Grid -> (Grid, Bool)
computeNextGeneration grid = (nextGrid, grid /= nextGrid)
  where
    nextGrid = nextGeneration grid

-- Function to read the initial grid from a file
readInitialGridFromFile :: FilePath -> IO Grid
readInitialGridFromFile filePath = do
  contents <- readFile filePath
  let rows = lines contents
  return $ map (map read . words) rows

-- Function to get the size of the grid
getGridSize :: Grid -> (Int, Int)
getGridSize grid = (length grid, length (head grid))

-- Modified main function to read the initial grid from a file
main :: IO ()
main = do
  putStrLn "Enter the number of desired iterations:"
  iterations <- readLn
  -- putStrLn "Enter the path to the file with the initial grid:"
  -- filePath <- getLine
  initialGrid <- readInitialGridFromFile "50x55.txt"
  let (rows, cols) = getGridSize initialGrid
  simulateGame initialGrid iterations
  


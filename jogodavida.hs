type Cell = Int
type Grid = [[Cell]]

countNeighbors :: Grid -> Int -> Int -> (Int, Int)
countNeighbors grid x y = (countLiveNeighbors grid x y, countZombieNeighbors grid x y)
  where
    countLiveNeighbors grid x y = length [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y, isValidIndex grid i j, grid !! i !! j == 1]
    countZombieNeighbors grid x y = length [(i, j) | i <- [x-1..x+1], j <- [y-1..y+1], i /= x || j /= y, isValidIndex grid i j, grid !! i !! j == 2]

isValidIndex :: Grid -> Int -> Int -> Bool
isValidIndex grid x y = x >= 0 && x < length grid && y >= 0 && y < length (head grid)

-- nextGeneration :: Grid -> Grid
-- nextGeneration grid = [[nextState grid i j | j <- [0..length (head grid) - 1]] | i <- [0..length grid - 1]]
--   where
--     nextState grid x y
--       | grid !! x !! y == 0 = if countLiveNeighbors grid x y == 3 then 1 else 0
--       | grid !! x !! y == 1 =
--         if countZombieNeighbors grid x y >= 1 || countLiveNeighbors grid x y < 2 || countLiveNeighbors grid x y > 3
--           then 0
--           else 1
--       | grid !! x !! y == 2 = if countLiveNeighbors grid x y == 0 then 0 else 2
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

-- Exemplo de uso
main :: IO ()
main = do
  putStrLn "Digite o tamanho da grade:"
  size <- readLn
  putStrLn "Digite o número de iterações desejadas:"
  iterations <- readLn
  putStrLn "Preencha a grade inicial (0 para célula morta, 1 para célula viva, 2 para zumbi):"
  initialGrid <- sequence [do putStrLn ("Fila " ++ show (i) ++ ":"); sequence [readLn | _ <- [1..size]] | i <- [1..size]]
  simulateGame initialGrid iterations

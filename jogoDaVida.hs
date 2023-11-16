import System.IO

vizinhos :: [[Int]] -> Int -> Int -> [Int]
vizinhos matriz i j = [ matriz !! x !! y | x <- [max 0 (i-1)..min (length matriz - 1) (i+1)], y <- [max 0 (j-1)..min (length (matriz !! x) - 1) (j+1)], x /= i || y /= j ]

aplicarRegras :: [[Int]] -> Int -> Int -> Int
aplicarRegras matriz i j
    | celula == 0 && vivas == 3 = 1
    | celula == 1 && (zumbis > 0 || vivas < 2 || vivas > 3) = if zumbis > 0 then 2 else 0
    | celula == 2 && vivas == 0 = 0
    | otherwise = celula
    where
        celula = matriz !! i !! j
        vizinhanca = vizinhos matriz i j
        vivas = length (filter (==1) vizinhanca)
        zumbis = length (filter (==2) vizinhanca)

iterar :: [[Int]] -> [[Int]]
iterar matriz = [ [ aplicarRegras matriz i j | j <- [0..length (matriz !! 0) - 1] ] | i <- [0..length matriz - 1] ]

main :: IO ()
main = do
    conteudo <- readFile "matriz.txt"
    let matriz = map (map read . words) (lines conteudo) :: [[Int]]
    putStrLn "Digite o número de iterações:"
    iteracoes <- readLn :: IO Int
    loop iteracoes matriz 1
    where
        loop 0 _ _ = return ()
        loop n matriz iter = do
            let novaMatriz = iterar matriz
            putStrLn ("Iteração " ++ show iter ++ ":")
            mapM_ (putStrLn . unwords . map show) novaMatriz
            if all (== 0) (concat novaMatriz)
                then return ()
                else loop (n-1) novaMatriz (iter+1)
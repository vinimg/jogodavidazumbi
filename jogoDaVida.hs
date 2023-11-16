import System.IO

vizinhos :: [[Int]] -> Int -> Int -> [Int]
vizinhos matriz i j = do
    let xs = [max 0 (i-1)..min (length matriz - 1) (i+1)]
    let ys = [max 0 (j-1)..min (length (head matriz) - 1) (j+1)]
    [ matriz !! x !! y | x <- xs, y <- ys, x /= i || y /= j ]

aplicarRegras :: [[Int]] -> Int -> Int -> Int
aplicarRegras matriz i j = do
    let celula = matriz !! i !! j
    let vizinhanca = vizinhos matriz i j
    let vivas = length (filter (==1) vizinhanca)
    let zumbis = length (filter (==2) vizinhanca)
    if celula == 0 && vivas == 3 then 1
    else if celula == 1 && (zumbis > 0 || vivas < 2 || vivas > 3) then if zumbis > 0 then 2 else 0
    else if celula == 2 && vivas == 0 then 0
    else celula

iterar :: [[Int]] -> [[Int]]
iterar matriz = do
    let linhas = [0..length matriz - 1]
    let colunas = [0..length (head matriz) - 1]
    [ [ aplicarRegras matriz i j | j <- colunas ] | i <- linhas ]

imprimirMatriz :: [[Int]] -> IO ()
imprimirMatriz = mapM_ (putStrLn . unwords . map show)


executarIteracoes :: Int -> [[Int]] -> Int -> IO ()
executarIteracoes iteracaoAtual matriz iteracoesMaximas = do
    if iteracaoAtual > iteracoesMaximas
        then return ()
        else do
            putStrLn ("Iteração " ++ show iteracaoAtual ++ ":")
            imprimirMatriz matriz
            if todosZeros matriz
                then return ()
                else executarIteracoes (iteracaoAtual + 1) (iterar matriz) iteracoesMaximas

todosZeros :: [[Int]] -> Bool
todosZeros matriz = all (== 0) (concat matriz)

main :: IO ()
main = do
    conteudo <- readFile "50x55.txt"
    let matriz = map (map read . words) (lines conteudo) :: [[Int]]
    putStrLn "Digite o número de iterações:"
    iteracoes <- readLn :: IO Int
    executarIteracoes 0 matriz iteracoes

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
    putStrLn ("Iteração " ++ show iteracaoAtual ++ ":")
    imprimirMatriz matriz
    let novaMatriz = iterar matriz
    if matriz == novaMatriz
        then putStrLn ("O sistema está estável na iteração " ++ show iteracaoAtual)
        else if iteracaoAtual >= iteracoesMaximas
            then putStrLn ("O sistema não estabilizou após " ++ show iteracoesMaximas ++ " iterações")
            else executarIteracoes (iteracaoAtual + 1) novaMatriz iteracoesMaximas



main :: IO ()
main = do
    putStrLn "Digite o formato da matriz (por exemplo, 10x10, 20x20):"
    formatoMatriz <- getLine
    let nomeArquivo = formatoMatriz ++ ".txt"
    conteudo <- readFile nomeArquivo
    let matriz = map (map read . words) (lines conteudo) :: [[Int]]
    putStrLn "Digite o número de iterações:"
    iteracoes <- readLn :: IO Int
    executarIteracoes 0 matriz iteracoes

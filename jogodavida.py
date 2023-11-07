def contar_vizinhos_vivos(grid, x, y):
    vizinhos_vivos = 0
    vizinhos_zumbis = 0
    for i in range(-1, 2):
        for j in range(-1, 2):
            if i == 0 and j == 0:
                continue
            if 0 <= x + i < len(grid) and 0 <= y + j < len(grid[0]):
                if grid[x + i][y + j] == 1:
                    vizinhos_vivos += 1
                elif grid[x + i][y + j] == 2:
                    vizinhos_zumbis += 1
    return vizinhos_vivos, vizinhos_zumbis

def proxima_geracao(grid):
    nova_grid = [[0 for _ in range(len(grid[0]))] for _ in range(len(grid))]
    mudou = False

    for i in range(len(grid)):
        for j in range(len(grid[0])):
            vizinhos_vivos, vizinhos_zumbis = contar_vizinhos_vivos(grid, i, j)

            if grid[i][j] == 0:  # célula morta
                if vizinhos_vivos == 3:
                    nova_grid[i][j] = 1
                    mudou = True
            elif grid[i][j] == 1:  # célula viva
                if vizinhos_zumbis >= 1 or vizinhos_vivos < 2 or vizinhos_vivos > 3:
                    nova_grid[i][j] = 0
                    mudou = True
                else:
                    nova_grid[i][j] = 1
            else:  # célula zumbi
                if vizinhos_vivos == 0:
                    nova_grid[i][j] = 0
                    mudou = True
                else:
                    nova_grid[i][j] = 2

    return nova_grid, mudou

def imprimir_grid(grid):
    for row in grid:
        print(' '.join(map(str, row)))

def jogo_da_vida_modificado(grid, num_iteracoes):
    for i in range(num_iteracoes):
        nova_grid, mudou = proxima_geracao(grid)
        print(f"Iteração {i + 1}:")
        imprimir_grid(nova_grid)
        if not mudou:
            print(f"Sistema estabilizado após {i + 1} iterações.")
            break
        grid = nova_grid

# Exemplo de uso
tamanho_grid = int(input("Digite o tamanho da grade: "))
num_iteracoes = int(input("Digite o número de iterações desejadas: "))
grid = [[0 for _ in range(tamanho_grid)] for _ in range(tamanho_grid)]

# Lógica para preencher o grid inicial, se necessário
# Exemplo de preenchimento manual:
# grid[1][1] = 1  # célula viva
# grid[2][2] = 2  # célula zumbi

jogo_da_vida_modificado(grid, num_iteracoes)

import random

def generate_matrix_file(filepath, rows, cols):
    matrix = [[random.choices([0, 1, 2], weights=[1, 2, 1], k=1)[0] for j in range(cols)] for i in range(rows)]
    with open(filepath, 'w') as f:
        for row in matrix:
            f.write(' '.join(map(str, row)) + '\n')

linha = 100
coluna = 200

generate_matrix_file(f'{linha}x{coluna}.txt', linha, coluna)

print(f'Matriz {linha}x{coluna} gerada com sucesso!')
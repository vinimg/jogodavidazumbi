import random

def generate_matrix_file(filepath, rows, cols):
    matrix = [[random.randint(0, 2) for j in range(cols)] for i in range(rows)]
    with open(filepath, 'w') as f:
        for row in matrix:
            f.write(' '.join(map(str, row)) + '\n')


generate_matrix_file('matrix1.txt', 10, 10)
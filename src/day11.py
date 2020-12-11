from copy import deepcopy
from pprint import pprint

def takestep(grid):
    n, m = len(grid), len(grid[0])
    newgrid = deepcopy(grid)
    for i, r in enumerate(grid):
        for j, c in enumerate(r):
            # print(i, j)
            neighbour_coords = [(i+r, j+c) for r in range(-1, 2) for c in range(-1, 2) if (r, c) != (0, 0)]
            # print(neighbour_coords)
            if c == 'L' and count_neighbours(grid, neighbour_coords, '#') == 0:
                newgrid[i][j] = '#'
            elif c == '#' and count_neighbours(grid, neighbour_coords, '#') >= 4:
                newgrid[i][j] = 'L'

    return newgrid


def takestep_2(grid):
    n, m = len(grid), len(grid[0])
    newgrid = deepcopy(grid)
    for i, r in enumerate(grid):
        for j, c in enumerate(r):
            # print(i, j)
            neighbour_coords = get_neighbour_coords(grid, (i, j))
            # print(neighbour_coords)
            if c == 'L' and count_neighbours(grid, neighbour_coords, '#') == 0:
                newgrid[i][j] = '#'
            elif c == '#' and count_neighbours(grid, neighbour_coords, '#') >= 5:
                newgrid[i][j] = 'L'

    return newgrid

def get_neighbour_coords(grid, point):
    n, m = len(grid), len(grid[0])
    i, j = point
    directions = [(r, c) for r in range(-1, 2) for c in range(-1, 2) if (r, c) != (0, 0)]
    neighbours = []
    for di, dj in directions:
        new_i, new_j = i+di, j+dj
        while 0 <= new_i < n and 0 <= new_j < m and grid[new_i][new_j] == '.':
            new_i += di
            new_j += dj

        neighbours.append((new_i, new_j))
    # print(neighbours)
    return neighbours


def count_neighbours(grid, coords, char):
    c = 0
    n, m = len(grid), len(grid[0])
    for i, j in coords:
        if not(0 <= i < n and 0 <= j < m):
            continue
        if grid[i][j] == char:
            c += 1
    return c

def countOccupied(grid):
    c = 0
    for r in grid:
        for seat in r:
            if seat == '#':
                c += 1

    return c

def print_grid(grid):
    print('\n'.join([''.join(row) for row in grid]))

def main():

    with open('data/day11.txt', 'r') as f:
        lines = f.readlines()

    grid = None
    newGrid = [list(line.strip()) for line in lines]
    while grid != newGrid:
        grid = newGrid
        newGrid = takestep_2(grid)
        # print_grid(newGrid)
        # print('---')
    print(countOccupied(newGrid))
main()

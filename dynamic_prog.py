#!/usr/bin/env python3

import sys


class Point:
    def __init__(self, i, j):
        self.i = i
        self.j = j

    def valid(self, problem):
        return self.i >= 0 and self.i < problem.rows and self.j >= 0 and self.j < problem.cols

    def __repr__(self):
        return 'Point(%d, %d)' % (self.i, self.j)


class Problem:
    def __init__(self, rows, cols, altitudes, targets, radius, num_ballons, turns, starting_cell, mov_grids):
        self.rows = rows
        self.cols = cols
        self.altitudes = altitudes
        self.targets = targets
        self.radius = radius
        self.num_ballons = num_ballons
        self.turns = turns
        self.starting_cell = starting_cell
        self.mov_grids = mov_grids


def parse_problem(f):
    rows, cols, altitudes = map(int, f.readline().strip().split())
    num_targets, radius, num_ballons, turns = map(int, f.readline().strip().split())
    starting_cell_i, starting_cell_j = map(int, f.readline().strip().split())

    targets = []
    for _ in range(num_targets):
        i, j = map(int, f.readline().strip().split())
        targets.append(Point(i, j))

    mov_grids = []
    mov_grids.append([]) # altitude 0
    for _ in range(altitudes):
        grid = []
        for _ in range(rows):
            in_ = list(map(int, f.readline().strip().split()))
            wind = []
            while in_:
                wind.append(Point(in_[0], in_[1]))
                in_ = in_[2:]

            grid.append(wind)

        mov_grids.append(grid)

    return Problem(rows, cols, altitudes, targets, radius, num_ballons, turns,
                   Point(starting_cell_i, starting_cell_j), mov_grids)


def solution_balloon(problem, starting_cell):
    possibilities = {}
    possibilities[starting_cell.i, starting_cell.j, 0] = 0
    #parent = [{} for _ in range(problem.turns)]

    # mask for covered cells
    mask_cells = []
    for i in range(-problem.radius, problem.radius + 1):
        for j in range(-problem.radius, problem.radius + 1):
            if i**2 + j**2 <= problem.radius**2:
                mask_cells.append((i, j))

    # map of targets
    target_map = [[False for _ in range(problem.cols)] for _ in range(problem.rows)]

    for t in problem.targets:
        target_map[t.i][t.j] = True

    # score map
    score_map = [[0 for _ in range(problem.cols)] for _ in range(problem.rows)]

    for i in range(problem.rows):
        for j in range(problem.cols):
            score_map[i][j] = 0

            for di, dj in mask_cells:
                if i + di >= 0 and i + di < problem.rows and target_map[i + di][(j + dj) % problem.cols]:
                    score_map[i][j] += 1

    for turn_id in range(problem.turns):
        new_possibilities = {}
        for (i, j, alt), score in possibilities.items():
            # no move #########
            nalt = alt
            ni = i + problem.mov_grids[nalt][i][j].i if nalt > 0 else i
            nj = (j + problem.mov_grids[nalt][i][j].j) % problem.cols if nalt > 0 else j

            if ni >= 0 and ni < problem.rows:
                nscore = score + score_map[ni][nj]

                if (ni, nj, nalt) not in new_possibilities or nscore > new_possibilities[ni, nj, nalt]:
                    new_possibilities[ni, nj, nalt] = nscore
                    #parent[turn_id][ni, nj, nalt] = (i, j, alt), 0

            # moving up #######
            if alt < problem.altitudes:
                nalt = alt + 1
                ni = i + problem.mov_grids[nalt][i][j].i
                nj = (j + problem.mov_grids[nalt][i][j].j) % problem.cols

                if ni >= 0 and ni < problem.rows:
                    nscore = score + score_map[ni][nj]

                    if (ni, nj, nalt) not in new_possibilities or nscore > new_possibilities[ni, nj, nalt]:
                        new_possibilities[ni, nj, nalt] = nscore
                        #parent[turn_id][ni, nj, nalt] = (i, j, alt), 1

            # moving down #######
            if alt >= 2:
                nalt = alt - 1
                ni = i + problem.mov_grids[nalt][i][j].i
                nj = (j + problem.mov_grids[nalt][i][j].j) % problem.cols

                if ni >= 0 and ni < problem.rows:
                    nscore = score + score_map[ni][nj]

                    if (ni, nj, nalt) not in new_possibilities or nscore > new_possibilities[ni, nj, nalt]:
                        new_possibilities[ni, nj, nalt] = nscore
                        #parent[turn_id][ni, nj, nalt] = (i, j, alt), -1

        possibilities = new_possibilities
        print('turn %d, %d possibilities' % (turn_id, len(possibilities)))

    print(possibilities)
    (i, j, alt), best_score = max(possibilities.items(), key=lambda v: v[1])
    print('best_score %d' % best_score)


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s FILE' % sys.argv[0], file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        problem = parse_problem(f)
        solution_balloon(problem, problem.starting_cell)

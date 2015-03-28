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

    def print(self):
        print('rows: %d' % self.rows)
        print('cols: %d' % self.cols)
        print('altitudes: %d' % self.altitudes)
        print('radius: %d' % self.radius)
        print('num_ballons: %d' % self.num_ballons)
        print('starting_cell: %s' % self.starting_cell)
        print('turns: %d' % self.turns)
        print('targets:')
        for t in self.targets[:5]:
            print('\t%s' % t)

        print('mov_grids[1]:')
        for wind in self.mov_grids[1][:2]:
            print('\t%s' % wind)

        print('mov_grids[2]:')
        for wind in self.mov_grids[2][:2]:
            print('\t%s' % wind)

        for a in range(1, self.altitudes + 1):
            assert(all(len(row) == self.cols for row in self.mov_grids[a]))


def score(problem, solution):
    # solution[turn][balloon_id] = {1, 0, -1}
    balloons_pos = [problem.starting_cell for _ in range(problem.num_ballons)]
    balloons_alt = [0 for _ in range(problem.num_ballons)]
    r = 0

    targets_set = set((target.i, target.j) for target in problem.targets)

    for turn_id in range(problem.turns):
        # déplacement en altitude
        for b_id in range(problem.num_ballons):
            pos = balloons_pos[b_id]
            if pos.valid(problem):
                if balloons_alt[b_id] == 0:
                    assert(solution[turn_id][b_id] == 0 or solution[turn_id][b_id] == 1)
                    balloons_alt[b_id] += solution[turn_id][b_id]
                else:
                    balloons_alt[b_id] += solution[turn_id][b_id]
                    assert(balloons_alt[b_id] > 0 and balloons_alt[b_id] <= problem.altitudes)

        # vent
        for b_id in range(problem.num_ballons):
            pos = balloons_pos[b_id]
            alt = balloons_alt[b_id]
            if alt > 0 and pos.valid(problem):
                i = pos.i + problem.mov_grids[alt][pos.i][pos.j].i
                j = (pos.j + problem.mov_grids[alt][pos.i][pos.j].j) % problem.cols
                balloons_pos[b_id] = Point(i, j)

        # on calcul les points du tour
        covered = set()
        for b_id in range(problem.num_ballons):
            pos = balloons_pos[b_id]
            if balloons_alt[b_id] > 0 and pos.valid(problem): # ballon valide
                for cell in covered_cells(problem, pos):
                    if (cell.i, cell.j) in targets_set:
                        covered.add((cell.i, cell.j))

        r += len(covered)

    return r


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


def parse_solution(problem, f):
    solution = []

    for _ in range(problem.turns):
        solution.append(list(map(int, f.readline().strip().split())))

    return solution


def column_dist(problem, c1, c2):
    diff = abs(c1 - c2)
    return min(diff, problem.cols - diff)


def covered_cells(problem, p):
    for i in range(p.i - problem.radius, p.i + problem.radius + 1):
        for j in range(p.j - problem.radius, p.j + problem.radius + 1):
            pos = Point(i, j % problem.cols)
            if pos.valid(problem) and (p.i - pos.i)**2 + column_dist(problem, p.j, pos.j)**2 <= problem.radius**2:
                yield pos


if __name__ == '__main__':
    if len(sys.argv) < 3:
        print('usage: %s INPUT OUTPUT' % sys.argv[0], file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as problem_f:
        with open(sys.argv[2], 'r') as solution_f:
            problem = parse_problem(problem_f)
            solution = parse_solution(problem, solution_f)
            print(score(problem, solution))

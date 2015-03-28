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


class Coord:
    def __init__(self, pt, alt):
        self.pt = pt
        self.alt = alt

    def __repr__(self):
        return 'Coord(%d, %d, %d)' % (self.pt.i, self.pt.j, self.alt)


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


class Solution:
    def __init__(self, problem):
        self.problem = problem
        self.balloons = [Coord(self.problem.starting_cell, 0) for _ in range(self.problem.num_ballons)]


def score(problem, solution):
    # solution[turn][balloon_id] = {1, 0, -1}
    balloons_pos = [problem.starting_cell for _ in range(problem.num_ballons)]
    balloons_alt = [0 for _ in range(problem.num_ballons)]
    r = 0

    for turn_id in problem.turns:
        # on calcul les points du tour
        for target in problem.targets:
            covered = False

            for b_id in range(problem.num_ballons):
                pos = balloons_pos[b_id]
                if balloons_alt[b_id] > 0 and pos.valid(problem): # ballon valide
                    if (pos.i - target.i)**2 + column_dist(pos.j, target.j)**2 <= problem.radius**2:
                        covered = True
                        break

            if covered:
                r += 1

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
            a = balloons_alt[b_id]
            if pos.valid(problem):
                i = pos.i + problem.mov_grids[a][pos.i][pos.j].i
                j = (pos.j + problem.mov_grids[a][pos.i][pos.j].j) % problem.cols
                balloons_alt[b_id] = Point(i, j)

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


def column_dist(problem, c1, c2):
    diff = abs(c1 - c2)
    return min(diff, problem.cols - diff)


def is_covered(problem, balloon, target):
    r, c = balloon.pt.i, balloon.pt.j
    u, v = target.i, target.j
    return (r - u)**2 + column_dist(c, v)**2 <= problem.radius**2


def nb_covered(solution):
    nb = 0
    for target in solution.prb.targets:
        for balloon in solution.balloons:
            if is_covered(solution.prb, balloon, target):
                nb += 1
                break
    return nb


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s FILE' % sys.argv[0], file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        p = parse_problem(f)
        p.print()

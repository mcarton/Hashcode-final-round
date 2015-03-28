#!/usr/bin/env python3

import sys


class Point:
    def __init__(self, i, j):
        self.i = i
        self.j = j

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


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s FILE' % sys.argv[0], file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        p = parse_problem(f)
        p.print()

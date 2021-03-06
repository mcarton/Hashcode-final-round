#!/usr/bin/env python3

import sys
import random
import copy

cover = []

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
    global cover
    # solution[turn][balloon_id] = {1, 0, -1}
    balloons_pos = [problem.starting_cell for _ in range(problem.num_ballons)]
    balloons_alt = [0 for _ in range(problem.num_ballons)]
    r = 0

    grid = [[-2 for _ in range(problem.cols)] for _ in range(problem.rows)]
    balloons_score = [0 for _ in range(problem.num_ballons)]
    tmp_point = Point(0,0)

    for turn_id in range(problem.turns):
        # déplacement en altitude
        for b_id in range(problem.num_ballons):
            pos = balloons_pos[b_id]
            if pos.valid(problem):
                balloons_alt[b_id] += solution[turn_id][b_id]

        # vent
        for b_id in range(problem.num_ballons):
            pos = balloons_pos[b_id]
            alt = balloons_alt[b_id]
            if alt > 0 and pos.valid(problem):
                i = pos.i + problem.mov_grids[alt][pos.i][pos.j].i
                j = (pos.j + problem.mov_grids[alt][pos.i][pos.j].j) % problem.cols
                balloons_pos[b_id] = Point(i, j)

        # on calcul les points du tour
        for b_id in range(problem.num_ballons):
            pos = balloons_pos[b_id]
            if balloons_alt[b_id] > 0 and pos.valid(problem): # ballon valide
                for cell in cover:
                    tmp_point.i = cell.i+pos.i
                    tmp_point.j = (cell.j+pos.j)%problem.cols
                    if tmp_point.valid(problem):
                        if grid[tmp_point.i][tmp_point.j] == -2:
                            grid[tmp_point.i][tmp_point.j] = b_id
                        else:
                            grid[tmp_point.i][tmp_point.j] = -1

        for target in problem.targets:
            target_status = grid[target.i][target.j]
            if target_status > -2:
                if target_status >= 0:
                    balloons_score[target_status] += 1
                r += 1
                grid[target.i][target.j] = -2

    return (r, balloons_score.index(min(balloons_score)))


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


def generate_solution(problem, solution):
    result = ''
    for turn in solution:
        result += ' '.join(str(move) for move in turn) + '\n'

    return result


def write_solution(problem, solution, filename):
    with open(filename, 'w') as f:
        f.write(generate_solution(problem, solution))


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


def random_balloon(problem, solution, b_id, iterations=None):
    max_solution = None
    max_score = 0

    if iterations is None:
        iterations = 10 ** 9

    for _ in range(iterations):
        alt = 0
        for turn_id in range(problem.turns):
            if alt == 0:
                solution[turn_id][b_id] = 1
            elif alt == problem.altitudes:
                solution[turn_id][b_id] = random.choice((-1, 0))
            elif alt == 1:
                solution[turn_id][b_id] = random.choice((0, 1))
            else:
                solution[turn_id][b_id] = random.choice((-1, 0, 1))

            alt += solution[turn_id][b_id]

        (s, _) = score(problem, solution)
        if s > max_score:
            max_score = s
            max_solution = copy.deepcopy(solution)

    return (max_solution, max_score)

def random_balloon_suite(problem, solution, b_id, iterations=None):
    max_solution = None
    max_score = 0
    max_min_b_id = b_id

    if iterations is None:
        iterations = 10 ** 9

    for _ in range(iterations):
        alt = 0
        for turn_id in range(problem.turns):
            if alt == 0:
                solution[turn_id][b_id] = 1
            elif alt == problem.altitudes:
                solution[turn_id][b_id] = random.choice((-1, 0))
            elif alt == 1:
                solution[turn_id][b_id] = random.choice((0, 1))
            else:
                solution[turn_id][b_id] = random.choice((-1, 0, 1))

            alt += solution[turn_id][b_id]

        (s, min_b_id) = score(problem, solution)
        if s > max_score:
            max_score = s
            max_solution = copy.deepcopy(solution)
            max_min_b_id = min_b_id

    return (max_solution, max_score, max_min_b_id)


def random_solution(problem, iterations):
    solution = [[0 for _ in range(problem.num_ballons)] for _ in range(problem.turns)]

    for b_id in range(problem.num_ballons):
        (solution, s) = random_balloon(problem, solution, b_id, iterations)
        print('current score %d: %d' % (b_id, s))

    (s, next_b_id) = score(problem, solution)
    solution_back = copy.deepcopy(solution)
    while True:
        (solution, next_s, next_b_id) = random_balloon_suite(problem, solution, next_b_id, iterations)
        if next_s > s:
            s = next_s
            write_solution(problem, solution, '%d.out' % s)
            solution_back = copy.deepcopy(solution)
        else:
            solution = solution_back
        print('current score + : %d' % s)

    return solution


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s FILE' % sys.argv[0], file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        problem = parse_problem(f)
        p = Point(0,0)
        for i in range(p.i - problem.radius, p.i + problem.radius + 1):
            for j in range(p.j - problem.radius, p.j + problem.radius + 1):
                pos = Point(i, j % problem.cols)
                if (p.i - pos.i)**2 + column_dist(problem, p.j, pos.j)**2 <= problem.radius**2:
                    cover.append(pos)
        solution = random_solution(problem, 300)
        s = score(problem, solution)
        write_solution(problem, solution, '%d.out' % s)

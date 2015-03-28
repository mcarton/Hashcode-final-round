#!/usr/bin/env python3

import sys


class Problem:
    def __init__(self):
        pass


def parse_problem(f):
    return Problem()


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('usage: %s FILE' % sys.argv[0], file=sys.stderr)
        exit(1)

    with open(sys.argv[1], 'r') as f:
        p = parse_problem(f)

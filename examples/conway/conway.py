import sys
from math import *
import time
import copy

RESET_CURSOR_POS = "\x1b[H"
SHOW_CURSOR = "\x1b[?25h"
HIDE_CURSOR = "\x1b[?25l"
CLEAR = "\x1b[2J"
ENDC = "\x1b[0m"

LIVE = "o"
DEAD = "."

MAX_H = 20
MAX_W = 40

def get_initial(path):
    with open(path, "r") as f:
        return [l[0:len(l)-1] for l in f.readlines()]


def make_map(initial):
    h = len(initial)
    w = len(initial[0])

    for i in range(1, h):
        l = len(initial[i])
        if l > w:
            w = l

    if h > MAX_H or w > MAX_W:
        return False

    ci_x = ceil(w/2) # center initial x
    ci_y = ceil(h/2) # center initial y

    cm_x = ceil(MAX_W/2)
    cm_y = ceil(MAX_H/2)

    m = [[DEAD]*MAX_W]*MAX_H
    for i in range(0, h):
        m[i + cm_y - ci_y] = list(DEAD*(cm_x - ci_x) + initial[i] + DEAD*(cm_x - ci_x+1))

    return m


def print_map(m):
    print(CLEAR, RESET_CURSOR_POS, end="")

    for i in range(0, MAX_H):
        print(''.join(m[i]))


def evolve_map(m):
    def get_num_live_neighbours(old, k, p):
        ct = 0
        dy = [-1, -1, 0, 1, 1,  1,  0, -1]
        dx = [ 0,  1, 1, 1, 0, -1, -1, -1]

        for i in range(0, 8):
            row = k+dy[i]
            col = p+dx[i]

            if row >= 0 and row < MAX_H and col >= 0 and col < MAX_W and LIVE == old[row][col]:
                ct = ct+1

        return ct

    old_map = copy.copy(m)

    for i in range(0, MAX_H):
        for j in range(0, MAX_W):
            n = get_num_live_neighbours(old_map, i, j)

            if LIVE == old_map[i][j]:
                if n < 2 or n > 3:
                    new = list(m[i])
                    new[j] = DEAD
                    m[i] = new
            elif n == 3:
                new = list(m[i])
                new[j] = LIVE
                m[i] = new

    return m


if __name__ == '__main__':
    if not sys.argv:
        print("Please provide a starting configuration")
        exit(1)


    m = make_map(get_initial(sys.argv[1]))

    print_map(m)

    if not m:
        print("Invalid starting configuration")
        exit(1)

    ct = 0
    print(HIDE_CURSOR, end="")
    while ct < 100:
        print_map(m)
        m = evolve_map(m)
        time.sleep(1)
        ct += 1

    print(SHOW_CURSOR, end="")

from itertools import islice
from collections import defaultdict


def iter_nth(it, n):
    return next(islice(it, n, n+1))

def manhattan_distance(point1, point2):
    return abs(point2[0]-point1[0]) + \
           abs(point2[1]-point1[1])


def add_points(p1, p2):
    x1, y1 = p1
    x2, y2 = p2
    return (x1+x2, y1+y2)


RIGHT = (1, 0)
UP    = (0, 1)
LEFT  = (-1, 0)
DOWN  = (0, -1)
UP_RIGHT   = add_points(RIGHT, UP)
UP_LEFT    = add_points(LEFT, UP)
DOWN_RIGHT = add_points(RIGHT, DOWN)
DOWN_LEFT  = add_points(LEFT, DOWN)


def neighbors8(point):
    return [ 
        add_points(point, RIGHT),
        add_points(point, UP),
        add_points(point, LEFT),
        add_points(point, DOWN),
        add_points(point, UP_RIGHT),
        add_points(point, UP_LEFT),
        add_points(point, DOWN_RIGHT),
        add_points(point, DOWN_LEFT),
    ]

def spiral():
    def move(pos, d, steps):
        for _ in range(steps):
            pos[0] += d[0]
            pos[1] += d[1]
            yield tuple(pos)
    pos = [0, 0]
    step = 1
    yield tuple(pos)
    while True:
        yield from move(pos, RIGHT, step)
        yield from move(pos, UP, step)
        step += 1
        yield from move(pos, LEFT, step)
        yield from move(pos, DOWN, step)
        step += 1

def day3_a():
    n = 368078 - 1
    nth_point = iter_nth(spiral(), n)
    return manhattan_distance((0, 0), nth_point)


def spiral_aggregate():
    value = defaultdict(int)
    for point in spiral():
        total = sum(value[p] for p in neighbors8(point))
        value[point] = max(total, 1)
        yield value[point]

def day3_b():
    for i in spiral_aggregate():
        if i > 368078:
            return i

print(day3_a())
print(day3_b())


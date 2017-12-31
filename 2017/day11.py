def directions():
    with open('data/input11.txt') as f:
        return f.read().rstrip().split(',')

def day11(dir):
    dir_map = {
        'n' : (0, -1), 'nw': (-1, -1), 'ne': (1, 0),
        'sw': (-1, 0), 'se': (1, 1),   's' : (0, 1)
    }
    p_x = p_y = 0
    furthest = 0
    for d in dir:
        p_x += dir_map[d][0]
        p_y += dir_map[d][1]
        furthest = max(furthest, max(abs(p_x), abs(p_y)))
    return max(abs(p_x), abs(p_y)), furthest

print(day11(directions()))

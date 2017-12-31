import re

def sanitize(rhs):
    for i, v in enumerate(rhs):
        rhs[i] = re.findall(r'(\d+)', v)[0]
    return rhs

def programs():
    with open('data/input12.txt') as f:
        lines = [line.split() for line in f.read().splitlines()]
        return {l: {l} | set(sanitize(r)) for (l, _, *r) in lines}

def reconstruct_path(came_from, src, dst):
    path = set()
    while dst in came_from:
        path.add(dst)
        if dst == came_from[dst]:
            break
        dst = came_from[dst]
    return tuple(sorted(path))

def has_path(adj_list, src, dst='0'):
    visited = set()
    stack = [src]
    came_from = {}
    while len(stack):
        v = stack.pop(0)
        if v == dst:
            return True
        for neighbor in adj_list[v]:
            if neighbor not in visited:
                came_from[neighbor] = src
                stack.append(neighbor)
        visited.add(v)
    return False

def day12_a(adj_list):
    return len(list(
        filter(None, [has_path(adj_list, src) for src in adj_list.keys()])
    ))

def day12_b(adj_list):
    graph = dict(adj_list)
    for s in graph:
        for e in list(graph[s]):
            if s != e:
                graph[s].update(graph[e])
                graph[e] = graph[s]
    paths = set()
    for v in graph:
        paths.add(tuple(sorted(graph[v])))
    return len(paths)

adj_list = programs()
print('A: ', day12_a(adj_list))
print('B: ', day12_b(adj_list))

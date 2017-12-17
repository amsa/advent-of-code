import re

class Node:
    def __init__(self, name, weight, children, parent=None):
        self.name = name
        self.weight = self.total_weight = weight
        self.children = children
        self.parent = parent

    def __str__(self):
        pn = self.parent.name if self.parent else None
        return 'Node(name={}, weight={} (total={}), children={}, parent={})'.\
            format(self.name, self.weight, self.total_weight, self.children, pn)

    def siblings(self):
        # root has no siblings
        if not self.parent:
            return []
        return [c for c in self.parent.children if c != self.name]


def get_nodes():
    raw = open('data/input7.txt').read().splitlines()
    def parse_line(line):
        name, weight, *children = re.findall(r'\w+', line)
        return Node(name, int(weight), children, parent=None)
    nodes = {n.name: n for n in map(parse_line, raw)}
    # set parent nodes
    set_parents(nodes)
    return nodes

def set_parents(nodes):
    for node in nodes.values():
        for ch in node.children:
            nodes[ch].parent = node


def day7_a():
    nodes = get_nodes()
    # find the root node (no parent)
    return next(filter(lambda x: x.parent == None, nodes.values()))

def day7_b(root):
    # aggregate weights for internal nodes
    def sum_weights(node):
        for c in node.children:
            node.total_weight += sum_weights(nodes[c])
        return node.total_weight

    nodes = get_nodes()
    # aggregate weights
    sum_weights(nodes[root])

    # find the node that needs to be balanced (bottom-up BFS)
    queue = [nodes[n] for n in nodes if len(nodes[n].children) == 0]
    while len(queue) > 0:
        node = queue.pop(0)
        for s in node.siblings():
            if node.total_weight > nodes[s].total_weight:
                # the parent node is the unbalanced node that needs to be balanced
                diff = node.total_weight - nodes[s].total_weight
                return node.weight - diff
        if node.parent not in queue:
            queue.append(node.parent)

print('A:', day7_a())
print('B:', day7_b('veboyvy'))

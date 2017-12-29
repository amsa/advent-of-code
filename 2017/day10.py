from itertools import cycle, islice
from functools import reduce

def get_lengths():
    with open('data/input10.txt') as f:
        return map(int, f.read().rstrip().split(','))

def get_ascii_lengths():
    with open('data/input10.txt') as f:
        return list(map(ord, f.read().rstrip())) + [17, 31, 73, 47, 23]

def knot_hash(array, lengths, count=1):
    skip   = 0
    pos    = 0
    for c in range(count):
        for l in lengths:
            stream = cycle(array)
            sublist = list(islice(stream, pos, pos + l))
            sublist = list(reversed(sublist))
            for i in range(pos, pos + l):
                array[i % len(array)] = sublist[i - pos]
            pos  += l + skip
            skip += 1
    return array


def day10_a(array, lengths):
    array = knot_hash(array, lengths)
    return array[0] * array[1]

def day10_b(array, lengths):
    array = knot_hash(array, lengths, 64)
    output = []
    for i in range(0, 256, 16):
        value = reduce(lambda x,y: x ^ y, array[i:i+16])
        output.append(hex(value)[-2:])
    return ''.join(output)

print(day10_a(list(range(0, 256)), get_lengths()))
print(day10_b(list(range(0, 256)), get_ascii_lengths()))

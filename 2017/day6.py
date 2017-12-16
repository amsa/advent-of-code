from itertools import count

def data():
    return tuple(map(int, open('data/input6.txt').read().split()))

def redistribute(banks):
    i = max(range(len(banks)), key=lambda x: banks[x])
    new_banks = list(banks)
    # set the max bank to 0
    new_banks[i] = 0
    for b in range(1, banks[i]+1):
        new_banks[(i+b) % len(banks)] += 1
    return tuple(new_banks)

assert redistribute((0, 2, 7, 0)) == (2, 4, 1, 2)
assert redistribute((2, 4, 1, 2)) == (3, 1, 2, 3)
assert redistribute((3, 1, 2, 3)) == (0, 2, 3, 4)

def day6(banks):
    states = {banks}
    for i in count(1):
        banks = redistribute(banks)
        if banks in states:
            return i, banks
        states.add(banks)
    return i

s, b = day6((0, 2, 7, 0))
assert s == 5

steps, new_bank = day6(data())
print('A:', steps)

steps, new_bank = day6(new_bank)
print('B:', steps)

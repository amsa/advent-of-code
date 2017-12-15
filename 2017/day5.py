def data():
    return tuple(map(int, open('data/input5.txt').read().splitlines()))

def day5_a(program):
    size = len(program)
    jump_list = list(program)
    steps = 0
    i = 0
    while i < size:
        jmp = jump_list[i]
        jump_list[i] += 1
        i += jmp
        steps += 1
    return steps

def day5_b(program):
    size = len(program)
    jump_list = list(program)
    steps = 0
    i = 0
    while i < size:
        jmp = jump_list[i]
        if jump_list[i] >= 3:
            jump_list[i] -= 1
        else:
            jump_list[i] += 1
        i += jmp
        steps += 1
    return steps

input_data = data()
print(day5_a(input_data))
print(day5_b(input_data))


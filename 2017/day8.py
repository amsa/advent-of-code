from collections import defaultdict
import re

def program():
    with open('data/input8.txt') as f:
        for line in f:
            reg, operator, operand, _, cond_reg, condition, cond_value = re.findall(r'\S+', line)
            op_map = {'inc': 1, 'dec': -1}
            yield reg, op_map[operator], int(operand), cond_reg, condition, int(cond_value)

def eval_condition(op1, operator, op2):
    return eval('{} {} {}'.format(op1, operator, op2))

def execute(registers, reg, operator, operand, cond_reg, condition, cond_value):
    if eval_condition(registers[cond_reg], condition, cond_value):
        registers[reg] += operator * operand


def day8_a():
    registers = defaultdict(int)
    for inst in program():
        execute(registers, *inst)
    # retrun the largest register value
    return max(registers.values())

def day8_b():
    max_reg = 0
    registers = defaultdict(int)
    for inst in program():
        execute(registers, *inst)
        max_reg = max(max(registers.values()), max_reg)
    return max_reg

print(day8_a())
print(day8_b())

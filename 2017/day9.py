import re

def stream(removeGarbage=True):
    with open('data/input9.txt') as f:
        s = re.sub(r'!.', '', f.read())
        if removeGarbage:
            return re.sub(r'<.*?>', '', s)
        return s

def day9_a(stream):
    total = levels = 0
    for c in stream:
        if c == '{':
            levels += 1
            total += levels
        elif c == '}':
            levels -= 1
    return total

def day9_b(stream):
    contents = re.findall(r'<([^>]*)>', stream)
    return sum(len(c) for c in contents)

assert day9_a('{}') == 1
assert day9_a('{{{}}}') == 6
assert day9_a('{{},{}}') == 5
assert day9_a('{{{},{},{{}}}}') == 16

print(day9_a(stream(True)))
print(day9_b(stream(False)))

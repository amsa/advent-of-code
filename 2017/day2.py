def head(iterable, default=None):
    return next(iter(iterable), default)

def day2_data(input_file_path='data/input2.txt'):
    with open(input_file_path) as f:
        return [tuple(map(int, line.strip().split('\t')))
                for line in f]

def day2_a():
    rows = day2_data()
    return sum(max(row) - min(row)
               for row in rows)

def day2_b():
    def find_divisibles(row):
        return head([i//j
                     for i in row
                     for j in row
                     if i > j and i % j == 0])
    rows = day2_data()
    return sum(map(find_divisibles, rows))


print(day2_a())
print(day2_b())

def day1_data(input_file_path='data/input1.txt'):
    with open(input_file_path) as f:
        line = f.readline().strip()
        return tuple(map(int, line))

def day1_a():
    numbers = day1_data()
    return sum(numbers[i]
               for i in range(len(numbers))
               if numbers[i] == numbers[i - 1])

def day1_b():
    numbers = day1_data()
    N = len(numbers)
    return sum(numbers[i]
               for i in range(N)
               if numbers[i] == numbers[i - N//2])



print(day1_a())
print(day1_b())

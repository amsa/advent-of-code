def day4_data():
    return open('data/input4.txt')

def is_valid_a(phrase):
    """
    Passphrase validator
    - no duplicate word is allowed in a passphrase
    """
    words = phrase.split()
    return len(words) == len(set(words))

def is_valid_b(phrase):
    """
    Passphrase validator
    - a valid passphrase cannot contain two words that are anagrams
    """
    words = phrase.split()
    sorted_words = set(map(lambda x: ''.join(sorted(x)), words))
    return len(words) == len(sorted_words)

def day4_a():
    phrases = day4_data()
    return len(list(filter(is_valid_a, phrases)))

def day4_b():
    phrases = day4_data()
    return len(list(filter(is_valid_b, phrases)))

print(day4_a())
print(day4_b())

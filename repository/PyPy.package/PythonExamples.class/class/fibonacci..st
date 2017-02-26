debugging
fibonacci: x
	Python pexec: '
def fibonacci(number):
    lookup_table = [0]
    for i in range(2, number + 1):
        lookup_table.append(lookup_table[i - 1] + lookup_table[i - 2])
    return lookup_table[number]'.
	^ Python eval: 'fibonacci(', x, ')'
debugging
average
	Python pexec: '
def average(iterable):
    return sum(iterable) / len(iterable)'.
	^ Python eval: 'average([])'
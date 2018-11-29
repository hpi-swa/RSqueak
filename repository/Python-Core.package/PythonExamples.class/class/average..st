debugging
average: anIterableDef
	Python exec: '
def average(iterable):
    return sum(iterable) / len(iterable)'.
	^ Python eval: 'average(', anIterableDef, ')'
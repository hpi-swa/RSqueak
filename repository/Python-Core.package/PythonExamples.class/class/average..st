debugging
average: anIterableDef
	Python pexec: '
def average(iterable):
    return sum(iterable) / len(iterable)'.
	^ Python eval: 'average(', anIterableDef, ')'
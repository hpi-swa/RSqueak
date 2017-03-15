as yet unclassified
testScopeEndInStartingAt
	| d input |
	d := ForeignLanguage.
	input := '
a = 2
def foo():
	x = 24
	def bar():
		return "bar"
	return "%s, %s" % (x, bar())
b = 4
x = b * a'.
	self assert: 9 equals: (d scopeEndIn: input startingAt: 1).
	self assert: 9 equals: (d scopeEndIn: input startingAt: 2).
	self assert: 7 equals: (d scopeEndIn: input startingAt: 3).
	self assert: 7 equals: (d scopeEndIn: input startingAt: 4).
	self assert: 6 equals: (d scopeEndIn: input startingAt: 5).
	self assert: 7 equals: (d scopeEndIn: input startingAt: 6).
	self assert: 9 equals: (d scopeEndIn: input startingAt: 8).
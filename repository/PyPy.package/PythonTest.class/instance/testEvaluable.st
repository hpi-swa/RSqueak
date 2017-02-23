testing
testEvaluable
	self assert: (Python evaluable: '1').
	self assert: (Python evaluable: 'foo()').
	self deny: (Python evaluable: 'x = 1').
	self assert: (Python evaluable: '1 == 1').
	self deny: (Python evaluable: 'def foo():
	return 42').
	self deny: (Python evaluable: 'def foo(): pass')
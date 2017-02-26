testing
testIsExpression
	self assert: (Python isExpression: '1').
	self assert: (Python isExpression: 'foo()').
	self deny: (Python isExpression: 'x = 1').
	self assert: (Python isExpression: '1 == 1').
	self deny: (Python isExpression: 'def foo():
	return 42').
	self deny: (Python isExpression: 'def foo(): pass').
	self deny: (Python isExpression: 'import os')
testing
testImmutabilityCollector
	| obj |
	obj := Object immutableNew.
	self assert: Object equals: obj targetClass.
	self assert: 0 equals: obj selectorList size.
	
	obj foo: 'x'.
	self assert: 1 equals: obj selectorList size.
	
	obj foo: 'y'.
	self assert: 2 equals: obj selectorList size.
	
	obj bar: 'x'.
	self assert: 3 equals: obj selectorList size
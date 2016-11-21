testing
testImmutableObjectFromLists
	| obj |
	
	obj := MyTestClass
			newWith: #(#aField: #anotherField: #someVar:someOtherVar:)
			and: #(#('foo') #(2) #('bar' true)).
	
	self assert: 'foo' equals: obj aField.
	self assert: 2 equals: obj anotherField.
	self assert: 'bar' equals: obj someVar1.
	self assert: obj someVar2
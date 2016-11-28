testing
testImmutableFrom
	| obj |
	
	obj := MyTestClass immutableFrom: (
		MyTestClass new
			aField: 'foo';
			anotherField: #(2);
			someVar: 'bar' someOtherVar: true).

	self assert: 'foo' equals: obj aField.
	self assert: #(2) equals: obj anotherField.
	self assert: 'bar' equals: obj someVar1.
	self assert: obj someVar2
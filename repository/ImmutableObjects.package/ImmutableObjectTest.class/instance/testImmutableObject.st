testing
testImmutableObject
	| obj |
	
	obj := MyTestClass immutableNew
		aField: 'foo';
		anotherField: #(2);
		someVar: 'bar' someOtherVar: true;
		commit.

	self assert: 'foo' equals: obj aField.
	self assert: #(2) equals: obj anotherField.
	self assert: 'bar' equals: obj someVar1.
	self assert: obj someVar2
testing
testImmutableObjectDNU
	| obj |
	obj := MyTestClass immutableNew unkownField: 'foo'.
	self should: [ obj commit ] raise: MessageNotUnderstood
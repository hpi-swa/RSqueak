test objects
objectUnderTest

	^ (self classUnderTest new: 3)
		aField: 'foo';
		anotherField: #(2);
		at: 1 put: true;
		at: 2 put: {};
		at: 3 put: 'bar';
		yourself
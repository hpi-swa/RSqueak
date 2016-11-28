test objects
objectUnderTest

	^ self classUnderTest new
		aField: 'foo';
		anotherField: #(2);
		someVar: 'bar' someOtherVar: true;
		yourself
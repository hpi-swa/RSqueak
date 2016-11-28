test objects
objectUnderTest

	^ (self classUnderTest new: 4)
		at: 1 put: 'foo';
		at: 2 put: #(2);
		at: 3 put: true
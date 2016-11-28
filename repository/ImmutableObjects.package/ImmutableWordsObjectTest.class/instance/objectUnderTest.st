test objects
objectUnderTest

	^ self classUnderTest withAll: ((1 to: 10) collect: [:i | i * 2])
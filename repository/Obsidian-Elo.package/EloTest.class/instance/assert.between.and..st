as yet unclassified
assert: expression between: lower and: upper
	| value |
	value := expression value.
	self assert: lower <= value.
	self assert: value <= upper.
as yet unclassified
testMapperSimple
	| mapper query object |
	mapper := SQLMapper using: self defaultDatabase.
	query := mapper execute: 'SELECT * FROM employees' mapping: SQLTestRecord.
	object := query next.
	
	self assert: object id equals: 1.
	self assert: object name equals: 'John Smith'.
	self assert: object salary equals: 12.5.

as yet unclassified
testCacheStatementConcurrent
	| database query1 query2 |
	
	database := self createDatabase: ':memory:'.
	self setUpDefault: database.
	
	query1 := database execute: 'SELECT * FROM employees'.
	query2 := database execute: 'SELECT * FROM employees'.
	
	self assert: query1 next equals: #(1 'John Smith' 12.5).
	self assert: query2 next equals: #(1 'John Smith' 12.5).
	self assert: query1 next equals: nil.
	self assert: query2 next equals: nil.

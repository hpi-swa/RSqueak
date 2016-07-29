as yet unclassified
testCacheStatementConcurrent
	| database query1 query2 |
	
	database := self createDatabase: ':memory:'.
	self setUpDefault: database.
	
	query1 := database execute: 'SELECT * FROM employees'.
	query2 := database execute: 'SELECT * FROM employees'.
	
	self assert: #(1 'John Smith' 12.5) equals: query1 next.
	self assert: #(1 'John Smith' 12.5) equals: query2 next.
	self assert: #(2 'Jane Smith' 16.7) equals: query1 next.
	self assert: #(2 'Jane Smith' 16.7) equals: query2 next.
	self assert: nil equals: query1 next.
	self assert: nil equals: query2 next.

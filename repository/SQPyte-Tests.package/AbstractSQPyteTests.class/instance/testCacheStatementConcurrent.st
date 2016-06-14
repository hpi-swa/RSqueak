as yet unclassified
testCacheStatementConcurrent
	| database query1 query2 |
	
	database := self createDatabase: ':memory:'.
	self setUpDefault: database.
	
	database execute: 'INSERT INTO records VALUES (?, ?, ?)' args: #(1 'John Smith' 12.5).
	query1 := database execute: 'SELECT * FROM records'.
	query2 := database execute: 'SELECT * FROM records'.
	
	self assert: query1 next equals: #(1 'John Smith' 12.5).
	self assert: query2 next equals: #(1 'John Smith' 12.5).
	self assert: query1 next equals: nil.
	self assert: query2 next equals: nil.
as yet unclassified
testSingleStatementInsert
	| database query |
	
	database := self createDatabase: ':memory:'.
	self setUpDefaultSchema: database.
	
	database execute: 'INSERT INTO employees VALUES (?, ?, ?)' args: #(1 'John Smith' 12.5).
	query := database execute: 'SELECT * FROM employees'.
	
	self assert: query next equals: #(1 'John Smith' 12.5).
	self assert: query next equals: nil.
